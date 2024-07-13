(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

(** Iterated register coallescing.

    Given the complexity of this algorithm, the implementation matches the
    pseudocode in the paper as closely as possible, including using the same
    names (even when they're arguably unclear).

    There is some duplication here with the implementation of {!Interf_graph},
    but I think it's valuable to isolate the implementation of a generic
    interference graph (which is likely a requirement for alternative register
    allocation algorithms) from the straightforward implementation of IRC. *)

open Fang_asm
open Fang_flow
open Fang_iter
open Fang_vm

module Edge = struct
  type t = box * box

  let normalize ((b1, b2) as t) = if Box.compare b1 b2 <= 0 then t else (b2, b1)

  let compare t1 t2 =
    let a1, a2 = normalize t1 and b1, b2 = normalize t2 in
    let c1 = Box.compare a1 b1 in
    if c1 <> 0 then c1 else Box.compare a2 b2
end

module Edge_set = Set.Make (Edge)

module Make (K : sig
  val precolored : Box.Set.t
  val spill_priority : graph -> box -> float
end) =
struct
  let k = Box.Set.cardinal K.precolored
  let is_precolored b = Box.Set.mem b K.precolored

  type state =
    { simplify_wl: Box.Set.t  (** Low-degree non-move-related nodes. *)
    ; freeze_wl: Box.Set.t  (** Low-degree move-related nodes. *)
    ; spill_wl: Box.Set.t  (** High-degree nodes. *)
    ; spilled_nodes: Box.Set.t
          (** Nodes marked for spilling during this round. *)
    ; coalesced_nodes: Box.Set.t
          (** Nodes that have been coalesced. When [v <- u] is coalesced, [v] is
              added to this set and [u] is put back into some worklist. *)
    ; colored_nodes: Box.Set.t  (** Nodes successfully colored. *)
    ; select_stack: box list
          (** Stack containing temporaries removed from the graph. *)
    ; coalesced_moves: Asm.Set.t
          (** Move instructions that have been coalesced. *)
    ; constrained_moves: Asm.Set.t
          (** Move instructions whose source and destination interfere. *)
    ; frozen_moves: Asm.Set.t
          (** Move instructions that will no longer be considered for
              coalescing.*)
    ; wl_moves: Asm.Set.t
          (** Move instructions enabled for possible coalescing. *)
    ; active_moves: Asm.Set.t
          (** Move instructions not yet ready for coalescing. *)
    ; adj_set: Edge_set.t  (** The set of interference edges in the graph. *)
    ; adj_list: Box.Set.t Box.Map.t
          (** Adjacency list representation of the graph. *)
    ; degree: int Box.Map.t  (** The current degree of each node. *)
    ; move_list: Asm.Set.t Box.Map.t
          (** A mapping from a node to the list of moves it is associated with. *)
    ; alias: box Box.Map.t
          (** When a move [v <- u] has been coalesced and [v] is put in
              {!coalesced_nodes} then the alias of [v] is [u]. *)
    ; color: box Box.Map.t
          (** The color chosen by the algorithm for a node. For precolored
              nodes, this is initialized to the given color. *)
    ; spill_priority: float Box.Map.t
          (** Nodes with a larger spill-priority should be chosen first to be
              spilled. *) }

  let build flow_graph interf =
    let adj_set = Iter.into_set (module Edge_set) (Interf_graph.edges interf) in
    let adj_list =
      let adj_list = ref Box.Map.empty in
      Edge_set.iter
        (fun (a, b) ->
          adj_list :=
            !adj_list
            |> Box.Map.update a (function
                 | None -> Some (Box.Set.singleton b)
                 | Some bs -> Some (Box.Set.add b bs) )
            |> Box.Map.update b (function
                 | None -> Some (Box.Set.singleton a)
                 | Some bs -> Some (Box.Set.add a bs) ) )
        adj_set ;
      !adj_list in
    let degree =
      let degree = ref Box.Map.empty in
      Interf_graph.vertices interf (fun b ->
          degree :=
            Box.Map.add b
              (if is_precolored b then max_int else Interf_graph.degree b interf)
              !degree ) ;
      !degree in
    let wl_moves, move_list =
      Iter.fold
        (Asm.Set.empty, Box.Map.empty)
        (fun (wl_moves, move_list) node ->
          match node with
          | `First (`Entry _ | `Label _) | `Last _ -> (wl_moves, move_list)
          | `Middle (`Asm s) ->
              Asm.case s
                ~move:(fun dst src ->
                  ( Asm.Set.add s wl_moves
                  , move_list
                    |> Box.Map.update dst (function
                         | None -> Some (Asm.Set.singleton s)
                         | Some ss -> Some (Asm.Set.add s ss) )
                    |> Box.Map.update src (function
                         | None -> Some (Asm.Set.singleton s)
                         | Some ss -> Some (Asm.Set.add s ss) ) ) )
                ~otherwise:(fun _ -> (wl_moves, move_list)) )
        (Graph.nodes flow_graph) in
    let color =
      Box.Map.of_seq (Box.Set.to_seq K.precolored |> Seq.map (fun r -> (r, r)))
    in
    let spill_priority =
      let spill_priority = ref Box.Map.empty in
      ( Interf_graph.vertices interf
      |> Iter.filter (fun b -> not (is_precolored b)) ) (fun b ->
          spill_priority :=
            Box.Map.add b (K.spill_priority flow_graph b) !spill_priority ) ;
      !spill_priority in
    { simplify_wl= Box.Set.empty
    ; freeze_wl= Box.Set.empty
    ; spill_wl= Box.Set.empty
    ; spilled_nodes= Box.Set.empty
    ; coalesced_nodes= Box.Set.empty
    ; colored_nodes= Box.Set.empty
    ; select_stack= []
    ; coalesced_moves= Asm.Set.empty
    ; constrained_moves= Asm.Set.empty
    ; frozen_moves= Asm.Set.empty
    ; wl_moves
    ; active_moves= Asm.Set.empty
    ; adj_set
    ; adj_list
    ; degree
    ; move_list
    ; alias= Box.Map.empty
    ; color
    ; spill_priority }

  let add_edge u v t =
    if (not (Edge_set.mem (u, v) t.adj_set)) && not (Box.equal u v) then
      let t = {t with adj_set= Edge_set.add (u, v) t.adj_set} in
      let t =
        if not (is_precolored u) then
          { t with
            adj_list=
              Box.Map.update u
                (function
                  | Some bs -> Some (Box.Set.add v bs) | None -> assert false )
                t.adj_list
          ; degree=
              Box.Map.update u
                (function Some d -> Some (d + 1) | None -> assert false)
                t.degree }
        else t in
      if not (is_precolored v) then
        { t with
          adj_list=
            Box.Map.update v
              (function
                | Some bs -> Some (Box.Set.add u bs) | None -> assert false )
              t.adj_list
        ; degree=
            Box.Map.update v
              (function Some d -> Some (d + 1) | None -> assert false)
              t.degree }
      else t
    else t

  let node_moves b t =
    Asm.Set.inter
      (try Box.Map.find b t.move_list with Not_found -> Asm.Set.empty)
      (Asm.Set.union t.active_moves t.wl_moves)

  let move_related b t = not (Asm.Set.is_empty (node_moves b t))

  let make_wl interf t =
    Interf_graph.vertices interf
    |> Iter.filter (fun b -> not (is_precolored b))
    |> Iter.fold t (fun t b ->
           if Box.Map.find b t.degree >= k then
             {t with spill_wl= Box.Set.add b t.spill_wl}
           else if move_related b t then
             {t with freeze_wl= Box.Set.add b t.freeze_wl}
           else {t with simplify_wl= Box.Set.add b t.simplify_wl} )

  let adjacent b t =
    Box.Set.diff
      (Box.Map.find b t.adj_list)
      (Box.Set.union (Box.Set.of_list t.select_stack) t.coalesced_nodes)

  let enable_moves bs t =
    Box.Set.fold
      (fun b t ->
        Asm.Set.fold
          (fun s t ->
            if Asm.Set.mem s t.active_moves then
              { t with
                active_moves= Asm.Set.remove s t.active_moves
              ; wl_moves= Asm.Set.add s t.wl_moves }
            else t )
          (node_moves b t) t )
      bs t

  let decrement_degree b t =
    let d = Box.Map.find b t.degree in
    let t =
      { t with
        degree=
          Box.Map.update b
            (function Some d -> Some (d - 1) | None -> assert false)
            t.degree } in
    if d = k then
      let t = enable_moves (Box.Set.add b (adjacent b t)) t in
      let t = {t with spill_wl= Box.Set.remove b t.spill_wl} in
      if move_related b t then {t with freeze_wl= Box.Set.add b t.freeze_wl}
      else {t with simplify_wl= Box.Set.add b t.simplify_wl}
    else t

  let simplify t =
    let b = Box.Set.min_elt t.simplify_wl in
    let t =
      { t with
        simplify_wl= Box.Set.remove b t.simplify_wl
      ; select_stack= b :: t.select_stack } in
    Box.Set.fold (fun a t -> decrement_degree a t) (adjacent b t) t

  let ok a b t =
    Box.Map.find a t.degree < k
    || is_precolored a
    || Edge_set.mem (a, b) t.adj_set

  let conservative bs t =
    let q =
      Box.Set.fold
        (fun b q -> if Box.Map.find b t.degree >= k then q + 1 else q)
        bs 0 in
    q < k

  let rec get_alias b t =
    if Box.Set.mem b t.coalesced_nodes then get_alias (Box.Map.find b t.alias) t
    else b

  let add_wl b t =
    if
      (not (is_precolored b))
      && (not (move_related b t))
      && Box.Map.find b t.degree < k
    then
      { t with
        freeze_wl= Box.Set.remove b t.freeze_wl
      ; simplify_wl= Box.Set.add b t.simplify_wl }
    else t

  let combine u v t =
    let t =
      if Box.Set.mem v t.freeze_wl then
        {t with freeze_wl= Box.Set.remove v t.freeze_wl}
      else {t with spill_wl= Box.Set.remove v t.spill_wl} in
    let t =
      { t with
        coalesced_nodes= Box.Set.add v t.coalesced_nodes
      ; alias= Box.Map.add v u t.alias
      ; move_list=
          (let vss =
             try Box.Map.find v t.move_list with Not_found -> Asm.Set.empty
           in
           Box.Map.update u
             (function
               | Some ss -> Some (Asm.Set.union ss vss) | None -> Some vss )
             t.move_list ) } in
    let t =
      Box.Set.fold
        (fun b t ->
          let t = add_edge b u t in
          decrement_degree b t )
        (adjacent v t) t in
    if Box.Map.find u t.degree >= k && Box.Set.mem u t.freeze_wl then
      { t with
        freeze_wl= Box.Set.remove u t.freeze_wl
      ; spill_wl= Box.Set.add u t.spill_wl }
    else t

  let coalesce t =
    let s = Asm.Set.min_elt t.wl_moves in
    let x, y =
      Asm.case s
        ~move:(fun x y -> (get_alias x t, get_alias y t))
        ~otherwise:(fun _ -> assert false) in
    let u, v = if is_precolored y then (y, x) else (x, y) in
    let t = {t with wl_moves= Asm.Set.remove s t.wl_moves} in
    if Box.equal u v then
      add_wl u {t with coalesced_moves= Asm.Set.add s t.coalesced_moves}
    else if is_precolored v || Edge_set.mem (u, v) t.adj_set then
      let t =
        add_wl u {t with constrained_moves= Asm.Set.add s t.constrained_moves}
      in
      add_wl v t
    else if
      (is_precolored u && Box.Set.for_all (fun b -> ok b u t) (adjacent v t))
      || (not (is_precolored u))
         && conservative (Box.Set.union (adjacent u t) (adjacent v t)) t
    then
      let t =
        combine u v {t with coalesced_moves= Asm.Set.add s t.coalesced_moves}
      in
      add_wl u t
    else {t with active_moves= Asm.Set.add s t.active_moves}

  let freeze_moves u t =
    Asm.Set.fold
      (fun s t ->
        let x, y =
          Asm.case s ~move:(fun x y -> (x, y)) ~otherwise:(fun _ -> assert false)
        in
        let v =
          if Box.equal (get_alias y t) (get_alias u t) then get_alias x t
          else get_alias y t in
        let t =
          { t with
            active_moves= Asm.Set.remove s t.active_moves
          ; frozen_moves= Asm.Set.add s t.frozen_moves } in
        if Asm.Set.is_empty (node_moves v t) && Box.Map.find v t.degree < k then
          { t with
            freeze_wl= Box.Set.remove v t.freeze_wl
          ; simplify_wl= Box.Set.add v t.simplify_wl }
        else t )
      (node_moves u t) t

  let freeze t =
    let b = Box.Set.min_elt t.freeze_wl in
    freeze_moves b
      { t with
        freeze_wl= Box.Set.remove b t.freeze_wl
      ; simplify_wl= Box.Set.add b t.simplify_wl }

  let select_spill t =
    let b =
      Iter.maximum
        (fun b1 b2 ->
          Float.compare
            ( Box.Map.find b1 t.spill_priority
            /. float_of_int (Box.Map.find b1 t.degree) )
            ( Box.Map.find b2 t.spill_priority
            /. float_of_int (Box.Map.find b2 t.degree) ) )
        (Iter.adapt Box.Set.iter t.spill_wl)
      |> Option.get in
    let t =
      { t with
        spill_wl= Box.Set.remove b t.spill_wl
      ; simplify_wl= Box.Set.add b t.simplify_wl } in
    freeze_moves b t

  let assign_colors t =
    let rec loop t =
      match t.select_stack with
      | [] -> t
      | b :: select_stack ->
          let t = {t with select_stack} in
          let ok_colors =
            Box.Set.fold
              (fun w ok_colors ->
                if
                  Box.Set.mem (get_alias w t)
                    (Box.Set.union t.colored_nodes K.precolored)
                then
                  Box.Set.remove
                    (Box.Map.find (get_alias w t) t.color)
                    ok_colors
                else ok_colors )
              (Box.Map.find b t.adj_list)
              K.precolored in
          let t =
            if Box.Set.is_empty ok_colors then
              {t with spilled_nodes= Box.Set.add b t.spilled_nodes}
            else
              { t with
                colored_nodes= Box.Set.add b t.colored_nodes
              ; color= Box.Map.add b (Box.Set.min_elt ok_colors) t.color } in
          loop t in
    let t = loop t in
    Box.Set.fold
      (fun b t ->
        (* n.b. This differs from the paper, which assumes that the alias of [b]
           does have a color. *)
        match Box.Map.find_opt (get_alias b t) t.color with
        | None -> t
        | Some alias_color -> {t with color= Box.Map.add b alias_color t.color}
        )
      t.coalesced_nodes t

  let main flow_graph interf =
    let t = build flow_graph interf in
    let t = make_wl interf t in
    let rec loop t =
      if not (Box.Set.is_empty t.simplify_wl) then loop (simplify t)
      else if not (Asm.Set.is_empty t.wl_moves) then loop (coalesce t)
      else if not (Box.Set.is_empty t.freeze_wl) then loop (freeze t)
      else if not (Box.Set.is_empty t.spill_wl) then loop (select_spill t)
      else t in
    let t = loop t in
    let t = assign_colors t in
    if not (Box.Set.is_empty t.spilled_nodes) then `Spill t.spilled_nodes
    else `Coloring (t.coalesced_moves, t.color)
end
