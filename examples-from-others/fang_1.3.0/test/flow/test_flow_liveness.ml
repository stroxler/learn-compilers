(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_flow
open Fang_iter
open Fang_vm

let%test_module _ =
  ( module struct
    module M = Liveness (struct let live_at_exit = Iter.one Rtl.c end)

    let graph =
      let l1 = local_label () and l2 = local_label () in
      let graph = Graph.(empty (global_label "snippet")) in
      let subgraph cursor =
        Graph.asm Rtl.(assign a (lit 0))
        @@ Graph.label l1 Rtl.jump
        @@ Graph.asm Rtl.(add b (reg a) (lit 1))
        @@ Graph.asm Rtl.(add c (reg c) (reg b))
        @@ Graph.asm Rtl.(mul a (reg b) (lit 2))
        @@ Graph.asm Rtl.(test a (lit 10))
        @@ Graph.cbranch Rtl.jlt l1 Rtl.jge l2
        @@ Graph.label l2 Rtl.jump cursor in
      Graph.blur (subgraph (Graph.entry graph))

    (* For reference. *)
    let%expect_test _ =
      Fmt.pr "%a" (Graph.pp Rtl.pp_reg) graph ;
      [%expect
        {|
        (frame
          (block (entry snippet)
                 (asm "a <- 0")
                 (branch "jump .L0"))
          (block
            (label .L0)
            (asm "b <- a + 1")
            (asm "c <- c + b")
            (asm "a <- b * 2")
            (asm "test a, 10")
            (cbranch "jlt .L0" "jge .L1"))
          (block (label .L1)
                 (exit))
          (block (label .L2)
                 (branch "jump .L1"))) |}]

    let liveness = M.run graph

    let pp_box_set =
      Fmt.braces (Fmt.iter ~sep:Fmt.comma Box.Set.iter Rtl.pp_reg)

    let test_live_in_nodes graph liveness =
      Format.open_vbox 0 ;
      Graph.blocks graph (fun block ->
          M.in_nodes block liveness (fun pair ->
              Fmt.pr "@[<h>%a@]@ "
                Fmt.(pair ~sep:comma (Node.pp Rtl.pp_reg) pp_box_set)
                pair ) ) ;
      Format.close_box ()

    let%expect_test _ =
      test_live_in_nodes graph liveness ;
      [%expect
        {|
        (branch "jump .L0"), {a, c}
        (asm "a <- 0"), {c}
        (entry snippet), {c}
        (cbranch "jlt .L0" "jge .L1"), {a, c}
        (asm "test a, 10"), {a, c}
        (asm "a <- b * 2"), {b, c}
        (asm "c <- c + b"), {b, c}
        (asm "b <- a + 1"), {a, c}
        (label .L0), {a, c}
        (exit), {c}
        (label .L1), {c}
        (branch "jump .L1"), {c}
        (label .L2), {c} |}]

    let test_live_out_nodes graph liveness =
      Format.open_vbox 0 ;
      Graph.blocks graph (fun block ->
          M.out_nodes block liveness (fun pair ->
              Fmt.pr "@[<h>%a@]@ "
                Fmt.(pair ~sep:comma (Node.pp Rtl.pp_reg) pp_box_set)
                pair ) ) ;
      Format.close_box ()

    let%expect_test _ =
      test_live_out_nodes graph liveness ;
      [%expect
        {|
        (branch "jump .L0"), {a, c}
        (asm "a <- 0"), {a, c}
        (entry snippet), {c}
        (cbranch "jlt .L0" "jge .L1"), {a, c}
        (asm "test a, 10"), {a, c}
        (asm "a <- b * 2"), {a, c}
        (asm "c <- c + b"), {b, c}
        (asm "b <- a + 1"), {b, c}
        (label .L0), {a, c}
        (exit), {c}
        (label .L1), {c}
        (branch "jump .L1"), {c}
        (label .L2), {c} |}]
  end )
