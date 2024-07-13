(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_asm
open Fang_flow
open Fang_iter
open Fang_vm
include Prelude

module type ALLOC = sig
  val alloc_without_handling_spills :
    graph -> [`Allocation of graph | `Spill of Box.Set.t]

  type spilling_handler

  val adjust_for_spills : stack_size:int -> spilling_handler -> asm -> asm list
  val alloc : stack_size:int -> graph -> graph * int
end

module Alloc (R : sig
  val all : box iter
  val special : box iter
  val preserved : box iter
end)
(P : SPILLING) =
struct
  module L = Liveness (struct
    let live_at_exit = Iter.concat R.special R.preserved
  end)

  let precolored = Iter.into_set (module Box.Set) R.all
  let is_precolored b = Box.Set.mem b precolored

  let mark_interference a b interf =
    if Box.equal a b || (is_precolored a && is_precolored b) then interf
    else Interf_graph.add_edge a b interf

  let build_interf_graph flow_graph =
    (* Each precolored register interferes with all the others. *)
    let interf =
      Iter.fold Interf_graph.empty
        (fun interf r1 ->
          Iter.fold interf
            (fun interf r2 ->
              if Box.equal r1 r2 then interf
              else Interf_graph.add_edge r1 r2 interf )
            R.all )
        R.all in
    let liveness = L.run flow_graph in
    Graph.blocks flow_graph
    |> Iter.fold interf (fun interf block ->
           L.out_nodes block liveness
           |> Iter.fold interf (fun interf (node, bs) ->
                  match node with
                  | `First (`Entry _ | `Label _) -> interf
                  | `Last _ -> interf
                  | `Middle (`Asm s) ->
                      let interf =
                        Iter.concat (Asm.use s) (Asm.def s)
                        |> Iter.fold interf (fun interf b ->
                               Interf_graph.add_vertex b interf ) in
                      Asm.case s
                        ~move:(fun a c ->
                          Box.Set.fold
                            (fun b interf ->
                              if not (Box.equal b c) then
                                mark_interference a b interf
                              else interf )
                            bs interf )
                        ~otherwise:(fun _ ->
                          Asm.def s
                          |> Iter.fold interf (fun interf a ->
                                 Box.Set.fold
                                   (fun b interf -> mark_interference a b interf)
                                   bs interf ) ) ) )

  module I = Irc.Make (struct
    let precolored = precolored let spill_priority _ _ = 1.0
  end)

  let alloc_without_handling_spills flow_graph =
    let interf = build_interf_graph flow_graph in
    match I.main flow_graph interf with
    | `Spill _ as r -> r
    | `Coloring (moves, colors) ->
        `Allocation
          ( flow_graph
          |> Graph.filter_map (fun s ->
                 if Asm.Set.mem s moves then None
                 else
                   let s = Asm.map (fun b -> Box.Map.find b colors) s in
                   Asm.case s
                     ~move:(fun dst src ->
                       if Box.equal dst src then None else Some s )
                     ~otherwise:(fun _ -> Some s) ) )

  type spilling_handler = P.handler

  let adjust_for_spills ~stack_size handler s =
    let use_spilled =
      Asm.use s |> Iter.filter (fun u -> P.was_spilled u handler) in
    let restores =
      Iter.map (fun u -> P.restore ~stack_size u handler) use_spilled in
    let def_spilled =
      Asm.def s |> Iter.filter (fun d -> P.was_spilled d handler) in
    let saves = Iter.map (fun d -> P.save ~stack_size d handler) def_spilled in
    let replacements =
      Iter.concat use_spilled def_spilled
      |> Iter.fold Box.Map.empty (fun replacements r ->
             Box.Map.add r (box ()) replacements ) in
    let ss = Iter.concat restores (Iter.cons s saves) in
    let ss_with_replacements =
      ss
      |> Iter.map (fun s ->
             Box.Map.fold
               (fun r b s -> Asm.replace ~target:r b s)
               replacements s ) in
    Iter.into_list ss_with_replacements

  let alloc ~stack_size flow_graph =
    (* First, we try to color the graph directly.

       If that fails due to a register spilling, the graph is rewritten and we
       try again.

       The less intrusive re-write, which happens first, is to use one of the
       callee-preserved registers by augmenting the graph to save it at the
       beginning and restore it at the end.

       If there are still spilled registers after all the preserved registers
       are in play, then more invasive rewrites happen by saving and restoring
       the spilled registers to the stack. *)
    let rec loop stack_size flow_graph available_preserved_regs =
      match alloc_without_handling_spills flow_graph with
      | `Allocation flow_graph -> (flow_graph, stack_size)
      | `Spill spilled -> (
        match available_preserved_regs with
        | r :: available_preserved_regs ->
            loop stack_size (P.preserve r flow_graph) available_preserved_regs
        | [] ->
            let handler = P.handler (Iter.adapt Box.Set.iter spilled) in
            let rewritten_graph =
              flow_graph
              |> Graph.flat_map (adjust_for_spills ~stack_size handler) in
            let stack_size = stack_size + P.stack_size_increase handler in
            loop stack_size rewritten_graph [] ) in
    loop stack_size flow_graph (Iter.into_list R.preserved)
end
