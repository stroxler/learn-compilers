(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_flow
open Fang_ir
open Fang_iter
open Fang_tiger
open Fang_translate
open Fang_x64

type error =
  [ `Parsing of parsing_error * source_span
  | `Validation of validation_error * source_span ]

let pp_error ppf = function
  | `Parsing (e, where) ->
      Fmt.pf ppf "@[<h>At@ %a:@ %a@]" Source_span.pp where pp_parsing_error e
  | `Validation (e, where) ->
      Fmt.pf ppf "@[<h>At@ %a:@ %a@]" Source_span.pp where e ()

module Compile_to_tiger (T : TIGER) = struct
  module M1 = Validation (T)
  module M0 = Parsing (M1)

  let apply lexbuf =
    match M0.parse lexbuf with
    | Error e -> Error (`Parsing e)
    | Ok expr -> (
      match M1.validate expr with
      | Error e -> Error (`Validation e)
      | Ok expr -> Ok expr )
end

module Compile_to_ir (I : IR) = struct
  module M2 = Translate (I)
  module M1 = Analysis (M2)
  module M0 = Compile_to_tiger (M1)

  let apply ?safe lexbuf =
    M0.apply lexbuf
    |> Result.map (fun expr ->
           let analysis, expr = M1.analyze expr in
           M2.translate ?safe analysis expr )
end

module Compile_to_canonical_ir (I : IR) = struct
  module M1 = Canonical (Lift (I))
  module M0 = Compile_to_ir (M1)

  let observe_frame_fragment (l, (v, stack_size)) =
    (l, (M1.observe v, stack_size))

  let apply ?safe lexbuf =
    M0.apply ?safe lexbuf
    |> Result.map (fun (frames, strings) ->
           (Iter.map observe_frame_fragment frames, strings) )
end

module Compile_to_flow = struct
  module M = Compile_to_canonical_ir (Emit)

  let apply ?safe lexbuf =
    M.apply ?safe lexbuf
    |> Result.map (fun (frames, strings) ->
           let graph_pairs =
             frames
             |> Iter.map (fun (l, (v, frame_ptr)) ->
                    (Graph.trace (Emit.flow_graph l v), frame_ptr) ) in
           (graph_pairs, strings) )
end

module Compile_to_allocated_flow = struct
  module M = Compile_to_flow

  let apply ?safe lexbuf =
    M.apply ?safe lexbuf
    |> Result.map (fun (graph_pairs, strings) ->
           ( Iter.map
               (fun (graph, frame_ptr) ->
                 let stack_size =
                   match frame_ptr with
                   | `No_frame_ptr -> 0
                   | `Frame_ptr stack_size -> stack_size in
                 let reg_graph, stack_size = Alloc.alloc ~stack_size graph in
                 let frame_ptr =
                   match (frame_ptr, stack_size) with
                   | `No_frame_ptr, 0 -> `No_frame_ptr
                   | `No_frame_ptr, _ | `Frame_ptr _, _ -> `Frame_ptr stack_size
                 in
                 (reg_graph, frame_ptr) )
               graph_pairs
           , strings ) )
end

let compile_to_tiger lexbuf =
  let module M = Compile_to_tiger (Pretty) in
  M.apply lexbuf |> Result.map (fun expr ppf () -> Pretty.pp ppf expr)

let pp_fragments ppf (frames, strings) =
  Fmt.(
    box
      ( const (vbox (Iter.pp (pp_frame_fragment pp_reg))) frames
      ++ sp
      ++ const (vbox (Iter.pp pp_string_fragment)) strings ))
    ppf ()

let compile_to_ir ?safe lexbuf =
  let module M = Compile_to_ir (Fang_ir.Pretty) in
  M.apply ?safe lexbuf
  |> Result.map (fun fragments ppf () -> pp_fragments ppf fragments)

let compile_to_canonical_ir ?safe lexbuf =
  let module M = Compile_to_canonical_ir (Fang_ir.Pretty) in
  M.apply ?safe lexbuf
  |> Result.map (fun fragments ppf () -> pp_fragments ppf fragments)

let compile_to_flow ?safe lexbuf =
  Compile_to_flow.apply ?safe lexbuf
  |> Result.map (fun (graph_pairs, _) ppf () ->
         Iter.pp (Graph.pp pp_reg) ppf (Iter.map fst graph_pairs) )

let compile_to_allocated_flow ?safe lexbuf =
  Compile_to_allocated_flow.apply ?safe lexbuf
  |> Result.map (fun (reg_graph_pairs, _) ppf () ->
         Iter.pp (Graph.pp pp_reg) ppf (Iter.map fst reg_graph_pairs) )

let compile_to_asm ?safe lexbuf =
  Compile_to_allocated_flow.apply ?safe lexbuf
  |> Result.map (fun (reg_graph_pairs, strings) ppf () ->
         Format.open_vbox 0 ;
         pp_asm_prefix ppf () ;
         reg_graph_pairs (fun (graph, frame_ptr) ->
             Fmt.(
               Format.pp_print_newline ++ Format.pp_print_newline
               ++ const (pp_asm frame_ptr) graph)
               ppf () ) ;
         strings (fun (l, s) ->
             Fmt.(
               Format.pp_print_newline ++ Format.pp_print_newline
               ++ const (pp_asm_string l) s)
               ppf () ) ;
         Format.close_box () )

let%test_module _ =
  ( module struct
    let prog =
      {|
let
  function make_greeting(name: string): string =
    concat(concat("Hello, ", name), "!")

  var p1 := "Joseph"
  var p2 := "Susan"
in
  print(make_greeting(p1));
  print_line();
  print(make_greeting(p2));
  print_line()
end
       |}

    let test fragment f =
      let lexbuf = Lexing.from_string fragment in
      match f lexbuf with
      | Ok pp -> Fmt.pr "%a@." pp ()
      | Error _ -> failwith "Unexpected error"

    let%expect_test "compile_to_tiger" =
      test prog compile_to_tiger ;
      [%expect
        {|
        let
          function make_greeting(name: string): string =
            concat(concat("Hello, ", name), "!")
          var p1 := "Joseph"
          var p2 := "Susan"
        in
          (print(make_greeting(p1));
           print_line();
           print(make_greeting(p2));
           print_line())
        end |}]

    let%expect_test "compile_to_ir" =
      test prog compile_to_ir ;
      [%expect
        {|
        (frame fang (fp 0)
          (perform
            (discard
              (perform
                (discard 0)
                (move #18 str2)
                (move #19 str3)
                (perform
                  (discard
                    (perform
                      (discard
                        (call fang_io_print (call fang.make_greeting rbp #18)))
                      (discard (call fang_io_print_line))
                      (discard
                        (call fang_io_print (call fang.make_greeting rbp #19)))
                      (perform (discard (call fang_io_print_line)) 0)))
                  0)))
            0))
        (frame fang.make_greeting no-fp
          (perform
            (move #17 rsi)
            (move #16 rdi)
            (call fang_string_concat (call fang_string_concat str0 #17) str1)))
        (string str1 "!")
        (string str0 "Hello, ")
        (string str2 "Joseph")
        (string str3 "Susan") |}]

    let%expect_test "compile_to_canonical_ir" =
      test prog compile_to_canonical_ir ;
      [%expect
        {|
        (frame fang (fp 0)
          (perform
            (move #24 str2)
            (move #25 str3)
            (move #26 (call fang.make_greeting rbp #24))
            (move #27 (call fang_io_print #26))
            (discard #27)
            (move #28 (call fang_io_print_line))
            (discard #28)
            (move #29 (call fang.make_greeting rbp #25))
            (move #30 (call fang_io_print #29))
            (discard #30)
            (move #31 (call fang_io_print_line))
            (discard #31)
            0))
        (frame fang.make_greeting no-fp
          (perform
            (move #21 rsi)
            (move #20 rdi)
            (move #22 (call fang_string_concat str0 #21))
            (move #23 (call fang_string_concat #22 str1))
            #23))
        (string str1 "!")
        (string str0 "Hello, ")
        (string str2 "Joseph")
        (string str3 "Susan") |}]

    let%expect_test "compile_to_flow" =
      test prog compile_to_flow ;
      [%expect
        {|
        (frame
          (block
            (entry fang)
            (asm "lea #47, str2")
            (asm "mov #36, #47")
            (asm "lea #46, str3")
            (asm "mov #37, #46")
            (asm "mov rdi, rbp")
            (asm "mov rsi, #36")
            (asm "call fang.make_greeting")
            (asm "mov #38, rax")
            (asm "mov rdi, #38")
            (asm "call fang_io_print")
            (asm "mov #39, rax")
            (asm "call fang_io_print_line")
            (asm "mov #40, rax")
            (asm "mov rdi, rbp")
            (asm "mov rsi, #37")
            (asm "call fang.make_greeting")
            (asm "mov #41, rax")
            (asm "mov rdi, #41")
            (asm "call fang_io_print")
            (asm "mov #42, rax")
            (asm "call fang_io_print_line")
            (asm "mov #43, rax")
            (asm "mov #45, 0")
            (asm "mov #44, #45")
            (asm "mov rax, #44")
            (exit)))
        (frame
          (block
            (entry fang.make_greeting)
            (asm "mov #33, rsi")
            (asm "mov #32, rdi")
            (asm "lea rdi, str0")
            (asm "mov rsi, #33")
            (asm "call fang_string_concat")
            (asm "mov #34, rax")
            (asm "mov rdi, #34")
            (asm "lea rsi, str1")
            (asm "call fang_string_concat")
            (asm "mov #35, rax")
            (asm "mov #48, #35")
            (asm "mov rax, #48")
            (exit))) |}]

    let%expect_test "compile_to_allocated_flow" =
      test prog compile_to_allocated_flow ;
      [%expect
        {|
        (frame
          (block
            (entry fang)
            (asm "push rbx")
            (asm "lea rsi, str6")
            (asm "lea rbx, str7")
            (asm "mov rdi, rbp")
            (asm "call fang.make_greeting")
            (asm "mov rdi, rax")
            (asm "call fang_io_print")
            (asm "call fang_io_print_line")
            (asm "mov rdi, rbp")
            (asm "mov rsi, rbx")
            (asm "call fang.make_greeting")
            (asm "mov rdi, rax")
            (asm "call fang_io_print")
            (asm "call fang_io_print_line")
            (asm "mov rax, 0")
            (asm "pop rbx")
            (exit)))
        (frame
          (block
            (entry fang.make_greeting)
            (asm "lea rdi, str4")
            (asm "call fang_string_concat")
            (asm "mov rdi, rax")
            (asm "lea rsi, str5")
            (asm "call fang_string_concat")
            (exit))) |}]

    let%expect_test "compile_to_asm" =
      test prog compile_to_asm ;
      [%expect
        {|
            .intel_syntax noprefix
            .global fang

        fang:
            push rbp
            mov rbp, rsp
            push rbx
            lea rsi, str10
            lea rbx, str11
            mov rdi, rbp
            call fang.make_greeting
            mov rdi, rax
            call fang_io_print
            call fang_io_print_line
            mov rdi, rbp
            mov rsi, rbx
            call fang.make_greeting
            mov rdi, rax
            call fang_io_print
            call fang_io_print_line
            mov rax, 0
            pop rbx
            leave
            ret

        fang.make_greeting:
            lea rdi, str8
            call fang_string_concat
            mov rdi, rax
            lea rsi, str9
            call fang_string_concat
            ret

        str9:
            .dc.a 1
            .string "!"

        str8:
            .dc.a 7
            .string "Hello, "

        str10:
            .dc.a 6
            .string "Joseph"

        str11:
            .dc.a 5
            .string "Susan" |}]
  end )
