(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_iter
open Fang_vm

(* Tests at the bottom will hopefully make the implementation concepts clear. *)

type head = First of Node.first | Head of head * Node.middle
type tail = Last of Node.last | Tail of Node.middle * tail
type t = Node.first * tail
type cursor = head * tail

(* TODO: Is this a reasonable way to define an ordering?

   The reason for the question: suppose there is a block [block0] and a block
   [block1] where [block1] is constructed by inserting nodes into [block0].
   [block0] and [block1] compare equal, but have different nodes.

   In a graph, we don't permit (by convention) blocks to be defined with the
   same label. That sort of side-steps the problem, but in an unsatisfying way.
   Also, what happens if we query a flow-analysis value with a block from a
   different graph than was analyzed? Oy. *)
let compare (first1, _) (first2, _) =
  match (first1, first2) with
  | `Entry l1, `Entry l2 -> Label.compare l1 l2
  | `Entry _, `Label _ -> -1
  | `Label _, `Entry _ -> 1
  | `Label l1, `Label l2 -> Label.compare l1 l2

let focus (first, tail) = (First first, tail)

let blur (head, tail) =
  let rec rewind head tail =
    match head with
    | First first -> (first, tail)
    | Head (head, middle) -> rewind head (Tail (middle, tail)) in
  rewind head tail

let nodes =
  let iter_nodes f (first, tail) =
    f (`First first) ;
    let rec loop = function
      | Last last -> f (`Last last)
      | Tail (middle, tail) ->
          f (`Middle middle) ;
          loop tail in
    loop tail in
  fun t -> Iter.adapt iter_nodes t

let last (head, tail) =
  let rec fast_forward head tail =
    match tail with
    | Last last -> (head, last)
    | Tail (middle, tail) -> fast_forward (Head (head, middle)) tail in
  fast_forward head tail

let successors =
  let iter_successors f t =
    match snd (last (focus t)) with
    | `Exit -> ()
    | `Branch (_, l) -> f l
    | `Cbranch (_, pos, _, neg) -> f pos ; f neg in
  fun t -> Iter.adapt iter_successors t

let pp pp_box ppf t =
  Fmt.pf ppf "@[<hv 2>(block@ @[<v>%a@])@]" (Iter.pp (Node.pp pp_box)) (nodes t)

let map_last f t =
  let head, last = last (focus t) in
  blur (head, Last (f last))

let filter_map f (first, tail) =
  let rec loop = function
    | Last _ as r -> r
    | Tail (`Asm s, tail) -> (
      match f s with None -> loop tail | Some s -> Tail (`Asm s, loop tail) )
  in
  (first, loop tail)

let flat_map f (first, tail) =
  let rec loop = function
    | Last _ as r -> r
    | Tail (`Asm s, tail') ->
        let rec roll = function
          | [] -> loop tail'
          | s :: ss -> Tail (`Asm s, roll ss) in
        roll (f s) in
  (first, loop tail)

module Key = struct
  type nonrec t = t

  let compare = compare
end

module Map = Map.Make (Key)
module Set = Set.Make (Key)

let%test_module _ =
  ( module struct
    open Fang_asm

    let l = global_label "foo"

    (* The empty block. *)
    let block : t = (`Entry l, Last `Exit)

    (* A cursor for the empty block. The focus is on the entry node, so nodes
       will be inserted immediately following it. *)
    let cursor : cursor = (First (`Entry l), Last `Exit)

    let%test _ = cursor = focus block

    let s body =
      Asm.v (fun ~use:_ ~def:_ _ ppf () -> (Fmt.box Fmt.string) ppf body)

    let s1 = s "aaa"
    and s2 = s "bbb"
    and s3 = s "ccc"
    and s4 = s "ddd"

    let show_asm s = Fmt.strf "%a" (Asm.pp Box.pp) s

    (* Insert [s1]. *)
    let cursor : cursor =
      let head, tail = cursor in
      (head, Tail (`Asm s1, tail))

    let%test _ =
      match cursor with
      | First (`Entry _), Tail (`Asm s1, Last `Exit) when show_asm s1 = "aaa" ->
          true
      | _ -> false

    (* Insert [s2] and [s3]. *)
    let cursor : cursor =
      let head, tail = cursor in
      (head, Tail (`Asm s3, Tail (`Asm s2, tail)))

    let%test _ =
      match cursor with
      | ( First (`Entry _)
        , Tail (`Asm s3, Tail (`Asm s2, Tail (`Asm s1, Last `Exit))) )
        when show_asm s3 = "ccc" && show_asm s2 = "bbb" && show_asm s1 = "aaa"
        ->
          true
      | _ -> false

    (* Shift the focus of the cursor to the first assembly node. *)

    let[@warning "-8"] cursor : cursor =
      let head, Tail (middle, tail) = cursor in
      (Head (head, middle), tail)

    let%test _ =
      match cursor with
      | ( Head (First (`Entry _), `Asm s3)
        , Tail (`Asm s2, Tail (`Asm s1, Last `Exit)) )
        when show_asm s3 = "ccc" && show_asm s2 = "bbb" && show_asm s1 = "aaa"
        ->
          true
      | _ -> false

    (* Insert [s4] after [s3]. *)
    let cursor : cursor =
      let head, tail = cursor in
      (head, Tail (`Asm s4, tail))

    (* Shift the focus one node again. *)
    let[@warning "-8"] cursor : cursor =
      let head, Tail (middle, tail) = cursor in
      (Head (head, middle), tail)

    let%test _ =
      match cursor with
      | ( Head (Head (First (`Entry _), `Asm s3), `Asm s4)
        , Tail (`Asm s2, Tail (`Asm s1, Last `Exit)) )
        when show_asm s3 = "ccc"
             && show_asm s4 = "ddd"
             && show_asm s2 = "bbb"
             && show_asm s1 = "aaa" ->
          true
      | _ -> false

    (* Convert the cursor back to a block. *)
    let block : t = blur cursor

    let%test _ =
      match block with
      | ( `Entry _
        , Tail
            (`Asm s3, Tail (`Asm s4, Tail (`Asm s2, Tail (`Asm s1, Last `Exit))))
        )
        when show_asm s3 = "ccc"
             && show_asm s4 = "ddd"
             && show_asm s2 = "bbb"
             && show_asm s1 = "aaa" ->
          true
      | _ -> false
  end )
