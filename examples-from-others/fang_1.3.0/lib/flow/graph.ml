(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_iter
open Fang_vm

type t = Block.Set.t

(** A zipper for the currently-focused block and a set of all the unfocused
    blocks. *)
type cursor = Block.cursor * t

type fragment = cursor -> cursor

let empty l = Block.Set.singleton (`Entry l, Last `Exit)
let focus block t = (Block.focus block, Block.Set.remove block t)
let entry t = focus (Block.Set.min_elt t) t

let exit t =
  Iter.adapt Block.Set.iter t
  |> Iter.filter_map (fun block ->
         let head, last = Block.last (Block.focus block) in
         match last with
         | `Exit ->
             let block_cursor = (head, Block.Last last) in
             Some (block_cursor, Block.Set.remove block t)
         | _ -> None )
  |> Iter.first |> Option.get

let blur (block_cursor, t) =
  let block = Block.blur block_cursor in
  Block.Set.add block t

let find_block_with_label l_needle t =
  Iter.adapt Block.Set.iter t
  |> Iter.find_first (function
       | `Label l, _ -> Label.equal l_needle l
       | _ -> false )
  |> Option.get

let iter_trace f t =
  let visited = ref Block.Set.empty in
  let rec loop block =
    visited := Block.Set.add block !visited ;
    f block ;
    Block.successors block (fun l ->
        let next_block = find_block_with_label l t in
        if not (Block.Set.mem next_block !visited) then loop next_block ) in
  loop (Block.Set.min_elt t)

let trace t = Iter.adapt iter_trace t |> Iter.into_set (module Block.Set)

let pp pp_box ppf t =
  Fmt.pf ppf "@[<hv 2>(frame@ @[<v>%a@])@]"
    (Fmt.iter Block.Set.iter (Block.pp pp_box))
    t

let filter_map f t = Block.Set.map (Block.filter_map f) t
let flat_map f t = Block.Set.map (Block.flat_map f) t
let blocks t = Iter.adapt Block.Set.iter t
let nodes t = blocks t |> Iter.flat_map Block.nodes
let asm s ((head, tail), t) = ((head, Block.Tail (`Asm s, tail)), t)

let label l m ((head, tail), t) =
  let block_cursor = (head, Block.Last (`Branch (m, l)))
  and block = (`Label l, tail) in
  (block_cursor, Block.Set.add block t)

let cbranch m pos m_inv neg ((head, tail), t) =
  let block_cursor = (head, Block.Last (`Cbranch (m, pos, m_inv, neg)))
  and block = (`Label (local_label ()), tail) in
  (block_cursor, Block.Set.add block t)

let branch m l ((head, tail), t) =
  let block_cursor = (head, Block.Last (`Branch (m, l)))
  and block = (`Label (local_label ()), tail) in
  (block_cursor, Block.Set.add block t)

module Label_map = Map.Make (Label)
module Label_set = Set.Make (Label)

(** We need to transform conditional branches so that they have a structure that
    lends itself to generating assembly.

    First, we trace the blocks of the graph, ensuring that the blocks are
    ordered based on the flow of execution.

    Next, we "flatten" all the blocks into a sequence of nodes. As a result,
    every conditional branch will either be followed by a label definition, or
    it will be the last node in the frame (since the block containing the [exit]
    node may not be in the last position).

    In the first case, we know that the label being defined will be one of the
    two labels in the conditional branch: the "positive" one or the "negative"
    one.

    If it's the negative label, then no further action is taken. The assembly
    for the conditional branch is just the instruction for conditionally jumping
    to the positive label. That is,

    {v
    cbranch m l1 m_inv l2
l2:
    stuff
    v}

    eventually becomes

    {v
    m l1
    stuff
    v}

    However, if it's the positive label then we need to "reverse" the
    conditional branch:

    {v
    cbranch m l1 m_inv l2
l1:
    stuff
    v}

    becomes

    {v
    cbranch m_inv l2 m l1
l1:
    stuff
    v}

    which eventually becomes

    {v
    m_inv l2
    stuff
    v}

    The other possibility is that the conditional branch is the last node in the
    frame. We have nothing to "fall-though" to here, so we need to insert a
    label to jump to.

    That is,

    {v
foo:
    stuff
    cbranch m l1 m_inv l2
    v}

    becomes

    {v
foo:
    stuff
    cbranch m l1 m_inv l3
l3:
    branch l2
    v}

    which eventually becomes

    {v
foo:
    stuff
    m l1
l3:
    branch l2
    v} *)
let tame_conditional_branches ~branch t =
  let reversed_nodes =
    t |> Iter.adapt iter_trace |> Iter.flat_map Block.nodes
    |> Iter.fold [] (fun nodes node ->
           match (nodes, node) with
           | `Last (`Cbranch (m, pos, m_inv, neg)) :: nodes, `First (`Label l)
             when Label.equal pos l ->
               (* A conditional branch is followed by the definition of [pos]. *)
               node :: `Last (`Cbranch (m_inv, neg, m, pos)) :: nodes
           | `Last (`Branch (_, l1)) :: nodes, `First (`Label l2)
             when Label.equal l1 l2 ->
               (* A branch to [l] is followed by the definition of [l]. We can
                  eliminate the branch. *)
               node :: nodes
           | _ -> node :: nodes ) in
  let reversed_nodes =
    match reversed_nodes with
    | `Last (`Cbranch (m, pos, m_inv, neg)) :: nodes ->
        (* A conditional branch can't be in the last position, because we have
           nothing to "fall-through" to. *)
        let l = local_label () in
        `Last (`Branch (branch, neg))
        :: `First (`Label l) :: `Last (`Cbranch (m, pos, m_inv, l)) :: nodes
    | _ -> reversed_nodes in
  Iter.adapt List.iter (List.rev reversed_nodes)

(** Sometimes, the flow-graph will be of the form

    {v
    cbranch m l1 m_inv l2
    stuff
l1:
    branch l3
l3:
   more stuff
    v}

    That is, [l1] "forwards" to [l3].

    Instead of getting to [l3] via [l1], we'd like to just branch to [l3]
    directly. *)
let remove_forwarding_labels t =
  let forwarding_labels = ref Label_map.empty in
  Block.Set.iter
    (function
      | `Label l1, Block.Last (`Branch (_, l2)) ->
          forwarding_labels := Label_map.add l1 l2 !forwarding_labels
      | _ -> () )
    t ;
  let rec resolve l1 =
    match Label_map.find_opt l1 !forwarding_labels with
    | Some l2 -> resolve l2
    | None -> l1 in
  t
  |> Block.Set.map
       (Block.map_last (function
         | `Branch (m, l) -> `Branch (m, resolve l)
         | `Cbranch (m, pos, m_inv, neg) ->
             `Cbranch (m, resolve pos, m_inv, resolve neg)
         | last -> last ) )

(** Note: [nodes] must be the nodes after conditional branches have been tamed! *)
let remove_orphan_labels nodes =
  let referenced_labels =
    Iter.fold Label_set.empty
      (fun referenced_labels node ->
        match node with
        | `Last (`Branch (_, l) | `Cbranch (_, l, _, _)) ->
            Label_set.add l referenced_labels
        | _ -> referenced_labels )
      nodes in
  nodes
  |> Iter.filter (function
       | `First (`Label l) -> Label_set.mem l referenced_labels
       | _ -> true )

let finalize ~branch t =
  t |> remove_forwarding_labels
  |> tame_conditional_branches ~branch
  |> remove_orphan_labels
