(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_alloc
open Fang_asm
open Fang_flow
open Fang_ir
open Fang_iter
open Fang_translate
open Fang_vm
include Prelude

(** What follows is an overview of the structure of the stack and frame
    pointers. In this diagram, every row corresponds to 4 B so that two rows
    correspond to a 64 b Fang word.

    Suppose that at this point in the execution of the program, the stack has
    room for 8 more words. Since the stack grows downward, the address of the
    last stack word is 0.

    The left side of the diagram is the memory address of each word.

    Note that the stack pointer ([sp]) is the address of the last word (not the
    address of the next word).

    In this example:

    - two parameters have been passed via the stack: [y1] and [y2]
    - there are three local variables on the stack: [x1], [x2], and [x3]

    {v
 64
     +--------+
 60  |        |
     +--------+
 56  |   y2   |  bp+24
     +--------+
 52  |        |
     +--------+
 48  |   y1   |  bp+16
     +--------+
 44  |        |
     +--------+
 40  |  sp0   |
     +--------+
 36  |        |
     +--------+
 32  |  bp0   |  bp
-----+--------+------
 28  |        |
     +--------+
 24  |   x1   |  bp-8
     +--------+
 20  |        |
     +--------+
 16  |   x2   |  bp-16
     +--------+
 12  |        |
     +--------+
  8  |   x3   |  sp = bp-24
     +--------+
  4  |        |
     +--------+
  0  |        |
     +--------+
    v}

    In the implementation below, all offsets are in units of words (not bytes)
    and assumed to grow downward. That is, an offset of 1 in the example above
    refers to [x1]. [y2] is at an offset of -2. *)
module Frame (I : IR) = struct
  type 'a ir = 'a I.t
  type handle = Box of box | Frame_offset of int

  type t =
    { args: handle array
    ; name: string
    ; label: label
    ; var_offset: int
          (** The offset of the latest local variable stored in the stack. *)
    ; param_offset: int
    ; prepare: effect ir option }

  let word_size = 8

  let combine_stores = function
    | [] -> None
    | store :: stores -> Some (List.fold_left I.seq store stores)

  let make name params =
    let rec loop hs var_offset param_offset stores rs params =
      match (params, rs) with
      | [], _ ->
          let prepare = combine_stores stores in
          { args= Array.of_list (List.rev hs)
          ; prepare
          ; name
          ; label= global_label name
          ; var_offset
          ; param_offset }
      | _ :: params, [] ->
          loop
            (Frame_offset (-(param_offset + 3)) :: hs)
            var_offset (param_offset + 1) stores rs params
      | `Local :: params, r :: rs ->
          let b = box () in
          let store = I.(move (box b) (box r)) in
          loop (Box b :: hs) var_offset param_offset (store :: stores) rs params
      | `Escapes :: params, r :: rs ->
          let store =
            I.(
              move
                (mem
                   (add (box bp)
                      (const
                         Int64.(
                           mul (of_int word_size)
                             (sub (neg (Int64.of_int var_offset)) one)) ) ) )
                (box r)) in
          loop
            (Frame_offset var_offset :: hs)
            (var_offset + 1) param_offset (store :: stores) rs params in
    loop [] 0 0 [] [di; si; dx; cx; r8; r9] params

  let label t = t.label
  let name t = t.name
  let address = I.box bp

  let alloc scope t =
    match scope with
    | `Local -> (Box (box ()), t)
    | `Escapes ->
        let h = Frame_offset t.var_offset in
        (h, {t with var_offset= t.var_offset + 1})

  let access ~address h =
    match h with
    | Box b -> I.box b
    | Frame_offset i ->
        let v = Int64.(mul (of_int word_size) (sub (neg (of_int i)) one)) in
        I.(mem (add address (const v)))

  let args t i = t.args.(i)

  let call_external name args =
    let l = global_label name in
    I.(call (loc l) args)

  let finalize v t =
    let v =
      match (t.args, t.prepare) with
      | [||], _ | _, None -> v
      | _, Some prepare -> I.perform prepare v in
    let needs_frame_ptr = t.param_offset <> 0 || t.var_offset <> 0 in
    let frame_ptr =
      if needs_frame_ptr then `Frame_ptr (t.var_offset * word_size)
      else `No_frame_ptr in
    (v, frame_ptr)
end

let%test_module "frame" =
  ( module struct
    module I = Canonical (Lift (Pretty))
    open Frame (I)

    let test_frame t =
      let body = I.const 0L in
      let v, fp = finalize body t in
      Fmt.pr "%a" (pp_frame_fragment pp_reg) (label t, (I.observe v, fp))

    let test_access h =
      let v = access ~address h in
      Fmt.pr "%a" (Pretty.pp pp_reg) (I.observe v)

    let%test_module "[Local variables]" =
      ( module struct
        let t = make "foo" []
        let h1, t = alloc `Local t
        let h2, t = alloc `Local t
        let h3, t = alloc `Local t

        let%expect_test _ = test_frame t ; [%expect {| (frame foo no-fp 0) |}]
        let%expect_test _ = test_access h1 ; [%expect {| #16 |}]
        let%expect_test _ = test_access h2 ; [%expect {| #17 |}]
        let%expect_test _ = test_access h3 ; [%expect {| #18 |}]
      end )

    let%test_module "[Escaping variables]" =
      ( module struct
        let t = make "foo" []
        let h1, t = alloc `Escapes t
        let h2, t = alloc `Escapes t
        let h3, t = alloc `Escapes t

        let%expect_test _ = test_frame t ; [%expect {| (frame foo (fp 24) 0) |}]
        let%expect_test _ = test_access h1 ; [%expect {| (mem (- rbp 8)) |}]
        let%expect_test _ = test_access h2 ; [%expect {| (mem (- rbp 16)) |}]
        let%expect_test _ = test_access h3 ; [%expect {| (mem (- rbp 24)) |}]
      end )

    let%test_module "[Local parameters passed by register]" =
      ( module struct
        let t = make "foo" [`Local; `Local; `Local]
        let h1 = args t 0
        let h2 = args t 1
        let h3 = args t 2

        let%expect_test _ =
          test_frame t ;
          [%expect
            {|
            (frame foo no-fp (perform (move #21 rdx)
                                      (move #20 rsi)
                                      (move #19 rdi) 0)) |}]

        let%expect_test _ = test_access h1 ; [%expect {| #19 |}]
        let%expect_test _ = test_access h2 ; [%expect {| #20 |}]
        let%expect_test _ = test_access h3 ; [%expect {| #21 |}]

        let%test_module "[Escaping parameters passed by register]" =
          ( module struct
            let t = make "foo" [`Escapes; `Escapes; `Escapes]
            let h1 = args t 0
            let h2 = args t 1
            let h3 = args t 2

            let%expect_test _ =
              test_frame t ;
              [%expect
                {|
                (frame foo (fp 24)
                  (perform
                    (move (mem (- rbp 24)) rdx)
                    (move (mem (- rbp 16)) rsi)
                    (move (mem (- rbp 8)) rdi)
                    0)) |}]

            let%expect_test _ = test_access h1 ; [%expect {| (mem (- rbp 8)) |}]

            let%expect_test _ =
              test_access h2 ; [%expect {| (mem (- rbp 16)) |}]

            let%expect_test _ =
              test_access h3 ; [%expect {| (mem (- rbp 24)) |}]
          end )

        let reg_params = [`Local; `Local; `Local; `Local; `Local; `Local]

        let%test_module "[Local parameters passed via the stack]" =
          ( module struct
            let t = make "foo" (reg_params @ [`Local; `Local; `Local])
            let h1 = args t (6 + 0)
            let h2 = args t (6 + 1)
            let h3 = args t (6 + 2)

            let%expect_test _ =
              test_frame t ;
              [%expect
                {|
                (frame foo (fp 0)
                  (perform
                    (move #27 r9)
                    (move #26 r8)
                    (move #25 rcx)
                    (move #24 rdx)
                    (move #23 rsi)
                    (move #22 rdi)
                    0)) |}]

            let%expect_test _ =
              test_access h1 ; [%expect {| (mem (+ rbp 16)) |}]

            let%expect_test _ =
              test_access h2 ; [%expect {| (mem (+ rbp 24)) |}]

            let%expect_test _ =
              test_access h3 ; [%expect {| (mem (+ rbp 32)) |}]
          end )

        let%test_module "[Escaping parameters passed via the stack]" =
          ( module struct
            let t = make "foo" (reg_params @ [`Escapes; `Escapes; `Escapes])
            let h1 = args t (6 + 0)
            let h2 = args t (6 + 1)
            let h3 = args t (6 + 2)

            let%expect_test _ =
              test_frame t ;
              [%expect
                {|
                (frame foo (fp 0)
                  (perform
                    (move #33 r9)
                    (move #32 r8)
                    (move #31 rcx)
                    (move #30 rdx)
                    (move #29 rsi)
                    (move #28 rdi)
                    0)) |}]

            let%expect_test _ =
              test_access h1 ; [%expect {| (mem (+ rbp 16)) |}]

            let%expect_test _ =
              test_access h2 ; [%expect {| (mem (+ rbp 24)) |}]

            let%expect_test _ =
              test_access h3 ; [%expect {| (mem (+ rbp 32)) |}]
          end )
      end )
  end )

module Translate (I : IR) = struct
  module F = Frame (I) include Translate (F) (I)
end

module Emit = Emit

module Spilling = struct
  let preserve b graph =
    let push =
      Asm.v ~use:[sp; b] ~def:[sp] (fun ~use ~def:_ pp_box ppf () ->
          Fmt.pf ppf "@[<h>push@ %a@]" pp_box (use 1) )
    and pop =
      Asm.v ~use:[sp] ~def:[b] (fun ~use:_ ~def pp_box ppf () ->
          Fmt.pf ppf "@[<h>pop@ %a@]" pp_box (def 0) ) in
    let graph = Graph.(blur (asm push (entry graph))) in
    let graph = Graph.(blur (asm pop (exit graph))) in
    graph

  type handler = int Box.Map.t

  let handler bs =
    let t = ref Box.Map.empty in
    let count = ref 0 in
    bs (fun b ->
        let i = !count in
        incr count ;
        t := Box.Map.add b i !t ) ;
    !t

  let was_spilled b t = Box.Map.mem b t

  let offset_of ~stack_size b t =
    let i = Box.Map.find b t in
    stack_size + (8 * (i + 1))

  let stack_size_increase t = 8 * Box.Map.cardinal t

  let save ~stack_size d t =
    Asm.v ~use:[bp; d] (fun ~use ~def:_ pp_box ppf () ->
        Fmt.pf ppf "@[<h>mov@ QWORD@ PTR@ [%a@ -@ %d],@ %a@]" pp_reg bp
          (offset_of ~stack_size d t)
          pp_box (use 1) )

  let restore ~stack_size u t =
    Asm.v ~use:[bp] ~def:[u] (fun ~use:_ ~def pp_box ppf () ->
        Fmt.pf ppf "@[<h>mov@ %a,@ QWORD@ PTR@ [%a@ -@ %d]@]" pp_box (def 0)
          pp_reg bp
          (offset_of ~stack_size u t) )
end

module Alloc = Alloc (Registers) (Spilling)

let pp_indented pp ppf () = Fmt.(hbox (sp ++ sp ++ sp ++ sp ++ pp)) ppf ()

let pp_asm_string l ppf s =
  let label = Fmt.(const pp_label l ++ const string ":") in
  let size =
    Fmt.(hbox (const string ".dc.a" ++ sp ++ const int (String.length s))) in
  let chars = Fmt.(hbox (const string ".string" ++ sp ++ const Dump.string s)) in
  Fmt.(vbox (label ++ sp ++ pp_indented size ++ sp ++ pp_indented chars)) ppf ()

let pp_asm = Asm.pp pp_reg

open struct
  let pp_first frame_ptr ppf first =
    match first with
    | `Entry l -> (
      match frame_ptr with
      | `No_frame_ptr -> Fmt.(const pp_label l ++ const string ":") ppf ()
      | `Frame_ptr stack_size ->
          let pp_entry =
            let preserve_frame_pointer ppf () =
              Fmt.pf ppf "@[<h>push@ %a@]" pp_reg bp in
            let set_frame_pointer ppf () =
              Fmt.pf ppf "@[<h>mov@ %a,@ %a@]" pp_reg bp pp_reg sp in
            let reserve_stack_space ppf () =
              Fmt.pf ppf "@[<h>sub@ %a,@ %d@]" pp_reg sp stack_size in
            pp_indented
              Fmt.(
                vbox
                  ( preserve_frame_pointer ++ sp ++ set_frame_pointer
                  ++
                  if stack_size <> 0 then sp ++ reserve_stack_space else Fmt.nop
                  )) in
          Fmt.(vbox (const pp_label l ++ const string ":" ++ sp ++ pp_entry))
            ppf () )
    | `Label l -> Fmt.pf ppf "%a:" pp_label l

  let pp_middle ppf = function
    | `Asm s -> pp_indented Fmt.(const pp_asm s) ppf ()

  let pp_last frame_ptr ppf = function
    | `Exit ->
        Format.pp_open_vbox ppf 0 ;
        ( match frame_ptr with
        | `Frame_ptr _ ->
            pp_indented Fmt.(const string "leave") ppf () ;
            Fmt.sp ppf ()
        | `No_frame_ptr -> () ) ;
        pp_indented Fmt.(const string "ret") ppf () ;
        Format.pp_close_box ppf ()
    | `Cbranch (m, l, _, _) | `Branch (m, l) ->
        pp_indented Fmt.(const pp_asm (m l)) ppf ()

  let pp_node frame_ptr ppf = function
    | `First first -> pp_first frame_ptr ppf first
    | `Middle middle -> pp_middle ppf middle
    | `Last last -> pp_last frame_ptr ppf last
end

let pp_asm_prefix ppf () =
  let select_syntax =
    Fmt.(hbox (const string ".intel_syntax" ++ sp ++ const string "noprefix"))
  in
  let export_symbol =
    Fmt.(hbox (const string ".global" ++ sp ++ const string "fang")) in
  pp_indented Fmt.(vbox (select_syntax ++ sp ++ export_symbol)) ppf ()

let pp_asm frame_ptr ppf graph =
  let finalized_graph = Graph.finalize ~branch:jump_asm graph in
  Fmt.pf ppf "@[<v>%a@]" (Iter.pp (pp_node frame_ptr)) finalized_graph
