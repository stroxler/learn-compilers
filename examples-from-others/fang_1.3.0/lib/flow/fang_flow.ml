(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_asm
open Fang_iter
open Fang_vm
module Node = Node

type node = Node.t

module Block = Block

type block = Block.t

module Graph = Graph

type graph = Graph.t

module type FACT = sig
  type t

  val initial : t
  val is_refinement : prev:t -> t -> bool
  val merge : t -> t -> t
end

module type RULE = sig
  type fact

  val first : fact -> Node.first -> fact
  val middle : fact -> Node.middle -> fact
  val last : (Block.t -> fact) -> Graph.t -> Node.last -> fact
end

module type ANALYSIS = sig
  type t
  type fact

  val run : Graph.t -> t
  val in_block : Block.t -> t -> fact
  val out_block : Block.t -> t -> fact
  val in_nodes : Block.t -> t -> (Node.t * fact) iter
  val out_nodes : Block.t -> t -> (Node.t * fact) iter
end

module Analysis (F : FACT) (R : RULE with type fact = F.t) = struct
  type fact = F.t
  type t = Graph.t * (Block.t -> fact)

  let propagate_fact_backwards block graph state =
    let head, last = Block.last (Block.focus block) in
    let last_fact =
      R.last (fun block -> Block.Map.find block state) graph last in
    let rec loop head fact =
      match head with
      | Block.First first -> R.first fact first
      | Block.Head (head, middle) -> loop head (R.middle fact middle) in
    let first_fact = loop head last_fact in
    let prev = Block.Map.find block state in
    if not (F.is_refinement ~prev first_fact) then (state, false)
    else (Block.Map.add block (F.merge prev first_fact) state, true)

  let run graph =
    let rec loop state =
      let state, was_refined =
        Graph.blocks graph
        |> Iter.fold (state, false) (fun (state, was_refined) block ->
               let state', was_refined' =
                 propagate_fact_backwards block graph state in
               (state', was_refined || was_refined') ) in
      if not was_refined then (graph, fun block -> Block.Map.find block state)
      else loop state in
    let state =
      Block.Set.to_seq graph
      |> Seq.map (fun block -> (block, F.initial))
      |> Block.Map.of_seq in
    loop state

  let in_block block (_, query) = query block

  let out_block block (graph, query) =
    let _, last = Block.last (Block.focus block) in
    R.last query graph last

  let in_nodes =
    let iter_in_nodes block f (graph, query) =
      let head, last = Block.last (Block.focus block) in
      let last_fact = R.last query graph last in
      f (`Last last, last_fact) ;
      let rec loop fact = function
        | Block.First first -> f (`First first, R.first fact first)
        | Block.Head (head, middle) ->
            let fact = R.middle fact middle in
            f (`Middle middle, fact) ;
            loop fact head in
      loop last_fact head in
    fun block t -> Iter.adapt (iter_in_nodes block) t

  let out_nodes =
    let iter_out_nodes block f (graph, query) =
      let head, last = Block.last (Block.focus block) in
      let last_fact = R.last query graph last in
      f (`Last last, last_fact) ;
      let rec loop fact = function
        | Block.First first -> f (`First first, fact)
        | Block.Head (head, middle) ->
            let fact' = R.middle fact middle in
            f (`Middle middle, fact) ;
            loop fact' head in
      loop last_fact head in
    fun block t -> Iter.adapt (iter_out_nodes block) t
end

module Live_regs = struct
  type t = Box.Set.t

  let initial = Box.Set.empty
  let is_refinement ~prev t = Box.Set.cardinal t > Box.Set.cardinal prev
  let merge = Box.Set.union
end

module Liveness (R : sig
  val live_at_exit : box iter
end) =
struct
  open struct
    module Live_regs_rule = struct
      type fact = Live_regs.t

      let into_set = Iter.into_set (module Box.Set)

      let update use def fact =
        Box.Set.union (into_set use) (Box.Set.diff fact (into_set def))

      let first fact = function `Entry _ | `Label _ -> fact
      let middle fact = function `Asm s -> update (Asm.use s) (Asm.def s) fact
      let at_exit = into_set R.live_at_exit

      let last query graph = function
        | `Terminate | `Exit -> at_exit
        | `Cbranch (_, pos, _, neg) ->
            let pos_block = Graph.find_block_with_label pos graph
            and neg_block = Graph.find_block_with_label neg graph in
            Box.Set.union (query pos_block) (query neg_block)
        | `Branch (_, l) ->
            let block = Graph.find_block_with_label l graph in
            query block
    end
  end

  include Analysis (Live_regs) (Live_regs_rule)
end
