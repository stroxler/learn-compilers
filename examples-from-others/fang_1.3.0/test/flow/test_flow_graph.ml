(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_asm
open Fang_flow
open Fang_iter
open Fang_vm

let%test_module _ =
  ( module struct
    let test t = (Graph.pp Rtl.pp_reg) Fmt.stdout t

    let%expect_test "empty" =
      test (Graph.empty (global_label "foo")) ;
      [%expect
        {|
        (frame (block (entry foo)
                      (exit))) |}]

    let%test_module "entry" =
      ( module struct
        let t = Graph.empty (global_label "foo")
        let cursor = Graph.entry t
        let cursor = Graph.asm Rtl.(assign a (lit 1)) cursor

        let%expect_test _ =
          test (Graph.blur cursor) ;
          [%expect
            {|
            (frame (block (entry foo)
                          (asm "a <- 1")
                          (exit))) |}]

        let cursor = Graph.asm Rtl.(assign a (lit 2)) cursor

        let%expect_test _ =
          test (Graph.blur cursor) ;
          [%expect
            {|
            (frame (block (entry foo)
                          (asm "a <- 2")
                          (asm "a <- 1")
                          (exit))) |}]

        let cursor = Graph.asm Rtl.(assign a (lit 3)) cursor

        let%expect_test _ =
          test (Graph.blur cursor) ;
          [%expect
            {|
            (frame (block (entry foo)
                          (asm "a <- 3")
                          (asm "a <- 2")
                          (asm "a <- 1")
                          (exit))) |}]
      end )

    let%test_module "exit" =
      ( module struct
        (* The behaviour of [exit] is only clear when the graph is non-empty.
           Otherwise, the cursor ends up in the same place as with [entry]! *)

        let t =
          let t = Graph.empty (global_label "foo") in
          let f cursor =
            Graph.asm Rtl.(assign a (lit 1))
            @@ Graph.asm Rtl.(assign a (lit 2))
            @@ Graph.asm Rtl.(assign a (lit 3)) cursor in
          Graph.blur (f (Graph.entry t))

        let cursor = Graph.exit t

        let%expect_test _ =
          test (Graph.blur (Graph.asm Rtl.(assign a (lit 0)) cursor)) ;
          [%expect
            {|
            (frame
              (block
                (entry foo)
                (asm "a <- 1")
                (asm "a <- 2")
                (asm "a <- 3")
                (asm "a <- 0")
                (exit))) |}]
      end )

    let l1 = local_label ()
    and l2 = local_label ()
    and b1 = box ()
    and b2 = box ()
    and b3 = box ()

    let t =
      let t = Graph.empty (global_label "multiply") in
      let f cursor =
        let open Graph in
        asm Rtl.(assign c (reg b3))
        @@ asm Rtl.(assign a (reg b1))
        @@ asm Rtl.(assign b (reg b2))
        @@ asm Rtl.(assign d (lit 0))
        @@ asm Rtl.(assign e (reg a))
        @@ label l1 Rtl.jump
        @@ asm Rtl.(add d (reg d) (reg b))
        @@ asm Rtl.(sub e (reg e) (lit 1))
        @@ asm Rtl.(test e (lit 0))
        @@ cbranch Rtl.jgt l1 Rtl.jle l2
        @@ label l2 Rtl.jump
        @@ asm Rtl.(assign b1 (reg d))
        @@ asm Rtl.(assign b3 (reg c)) cursor in
      Graph.blur (f (Graph.entry t))

    let%expect_test _ =
      test t ;
      [%expect
        {|
        (frame
          (block
            (entry multiply)
            (asm "c <- #23")
            (asm "a <- #21")
            (asm "b <- #22")
            (asm "d <- 0")
            (asm "e <- a")
            (branch "jump .L3"))
          (block
            (label .L3)
            (asm "d <- d + b")
            (asm "e <- e - 1")
            (asm "test e, 0")
            (cbranch "jgt .L3" "jle .L4"))
          (block (label .L4)
                 (asm "#21 <- d")
                 (asm "#23 <- c")
                 (exit))
          (block (label .L5)
                 (branch "jump .L4"))) |}]

    let%expect_test "blocks" =
      Fmt.pr "%a" (Fmt.vbox (Iter.pp (Block.pp Rtl.pp_reg))) (Graph.blocks t) ;
      [%expect
        {|
        (block
          (entry multiply)
          (asm "c <- #23")
          (asm "a <- #21")
          (asm "b <- #22")
          (asm "d <- 0")
          (asm "e <- a")
          (branch "jump .L3"))
        (block
          (label .L3)
          (asm "d <- d + b")
          (asm "e <- e - 1")
          (asm "test e, 0")
          (cbranch "jgt .L3" "jle .L4"))
        (block (label .L4)
               (asm "#21 <- d")
               (asm "#23 <- c")
               (exit))
        (block (label .L5)
               (branch "jump .L4")) |}]

    let%expect_test "nodes" =
      Fmt.pr "%a" (Fmt.vbox (Iter.pp (Node.pp Rtl.pp_reg))) (Graph.nodes t) ;
      [%expect
        {|
        (entry multiply)
        (asm "c <- #23")
        (asm "a <- #21")
        (asm "b <- #22")
        (asm "d <- 0")
        (asm "e <- a")
        (branch "jump .L3")
        (label .L3)
        (asm "d <- d + b")
        (asm "e <- e - 1")
        (asm "test e, 0")
        (cbranch "jgt .L3" "jle .L4")
        (label .L4)
        (asm "#21 <- d")
        (asm "#23 <- c")
        (exit)
        (label .L5)
        (branch "jump .L4") |}]

    let%expect_test "trace" =
      test (Graph.trace t) ;
      [%expect
        {|
        (frame
          (block
            (entry multiply)
            (asm "c <- #23")
            (asm "a <- #21")
            (asm "b <- #22")
            (asm "d <- 0")
            (asm "e <- a")
            (branch "jump .L3"))
          (block
            (label .L3)
            (asm "d <- d + b")
            (asm "e <- e - 1")
            (asm "test e, 0")
            (cbranch "jgt .L3" "jle .L4"))
          (block (label .L4)
                 (asm "#21 <- d")
                 (asm "#23 <- c")
                 (exit))) |}]

    let%expect_test "finalize" =
      Fmt.pr "%a"
        Fmt.(vbox (Iter.pp (Node.pp Rtl.pp_reg)))
        (Graph.finalize ~branch:Rtl.jump t) ;
      [%expect
        {|
        (entry multiply)
        (asm "c <- #23")
        (asm "a <- #21")
        (asm "b <- #22")
        (asm "d <- 0")
        (asm "e <- a")
        (label .L3)
        (asm "d <- d + b")
        (asm "e <- e - 1")
        (asm "test e, 0")
        (cbranch "jgt .L3" "jle .L4")
        (asm "#21 <- d")
        (asm "#23 <- c")
        (exit) |}]

    let%expect_test "filter_map" =
      (* Remove all instructions which assign to [e]. *)
      test
        (Graph.filter_map
           (fun s ->
             match
               Iter.find_first (fun b -> Box.compare b Rtl.e = 0) (Asm.def s)
             with
             | Some _ -> None
             | None -> Some s )
           t ) ;
      [%expect
        {|
        (frame
          (block
            (entry multiply)
            (asm "c <- #23")
            (asm "a <- #21")
            (asm "b <- #22")
            (asm "d <- 0")
            (branch "jump .L3"))
          (block
            (label .L3)
            (asm "d <- d + b")
            (asm "test e, 0")
            (cbranch "jgt .L3" "jle .L4"))
          (block (label .L4)
                 (asm "#21 <- d")
                 (asm "#23 <- c")
                 (exit))
          (block (label .L5)
                 (branch "jump .L4"))) |}]

    let%expect_test "flat_map" =
      (* Triple all instructions which read or write [a]. *)
      test
        (Graph.flat_map
           (fun s ->
             match
               Iter.find_first
                 (fun b -> Box.compare b Rtl.a = 0)
                 (Iter.concat (Asm.use s) (Asm.def s))
             with
             | Some _ -> [s; s; s]
             | None -> [s] )
           t ) ;
      [%expect
        {|
        (frame
          (block
            (entry multiply)
            (asm "c <- #23")
            (asm "a <- #21")
            (asm "a <- #21")
            (asm "a <- #21")
            (asm "b <- #22")
            (asm "d <- 0")
            (asm "e <- a")
            (asm "e <- a")
            (asm "e <- a")
            (branch "jump .L3"))
          (block
            (label .L3)
            (asm "d <- d + b")
            (asm "e <- e - 1")
            (asm "test e, 0")
            (cbranch "jgt .L3" "jle .L4"))
          (block (label .L4)
                 (asm "#21 <- d")
                 (asm "#23 <- c")
                 (exit))
          (block (label .L5)
                 (branch "jump .L4"))) |}]
  end )
