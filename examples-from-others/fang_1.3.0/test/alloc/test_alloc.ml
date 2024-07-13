(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_asm
open Fang_iter
open Fang_vm
open Fang_x64.Alloc

let pp_asm = Asm.pp Fang_x64.pp_reg

let print_spilling_adjustment ~spilled s =
  let handler = Fang_x64.Spilling.handler (Iter.adapt List.iter spilled) in
  let ss = adjust_for_spills ~stack_size:16 handler s in
  Fmt.pr "@[<h>%a,@ @[%a@]@]@." (Fmt.quote pp_asm) s
    Fmt.(quote (vbox (list pp_asm)))
    ss

let b0 = box ()
and b1 = box ()
and b2 = box ()

let spilled = [b1; b2]

let move dst src =
  Asm.v ~is_move:true ~use:[src] ~def:[dst] (fun ~use ~def pp_box ppf () ->
      Fmt.pf ppf "@[<h>mov@ %a,@ %a@]" pp_box (def 0) pp_box (use 0) )

let%expect_test "[Using a spilled register causes it to be restored first]" =
  print_spilling_adjustment ~spilled (move b0 b1) ;
  [%expect
    {|
    "mov #16, #17", "mov #19, QWORD PTR [rbp - 24]
                     mov #16, #19" |}]

let%expect_test "[Defining a spilled register causes it to be saved after]" =
  print_spilling_adjustment ~spilled (move b1 b0) ;
  [%expect
    {|
    "mov #17, #16", "mov #20, #16
                     mov QWORD PTR [rbp - 24], #20" |}]

let%expect_test "[An instruction can consist of used and defined registers]" =
  print_spilling_adjustment ~spilled (move b1 b2) ;
  [%expect
    {|
    "mov #17, #18", "mov #21, QWORD PTR [rbp - 32]
                     mov #22, #21
                     mov QWORD PTR [rbp - 24], #22" |}]

let%expect_test "[A spilled register can be both used and defined]" =
  print_spilling_adjustment ~spilled
    (Asm.v ~use:[b0; b1] ~def:[b1] (fun ~use ~def pp_box ppf () ->
         Fmt.pf ppf "@[<h>add@ %a,@ %a@]" pp_box (def 0) pp_box (use 0) ) ) ;
  [%expect
    {|
    "add #17, #16", "mov #24, QWORD PTR [rbp - 24]
                     add #24, #16
                     mov QWORD PTR [rbp - 24], #24" |}]

let%expect_test "[An instruction can have multiple spilled registers that are \
                 used and defined]" =
  print_spilling_adjustment ~spilled
    (Asm.v ~use:[b1; b2] ~def:[b2] (fun ~use ~def pp_box ppf () ->
         Fmt.pf ppf "@[<h>add@ %a,@ %a@]" pp_box (def 0) pp_box (use 0) ) ) ;
  [%expect
    {|
    "add #18, #17", "mov #25, QWORD PTR [rbp - 24]
                     mov #27, QWORD PTR [rbp - 32]
                     add #27, #25
                     mov QWORD PTR [rbp - 32], #27" |}]
