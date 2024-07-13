(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_asm
open Fang_flow
open Fang_ir
open Fang_iter
open Fang_vm
open Prelude

let unimplemented where = failwith (Fmt.strf "[%s]: Unimplemented" where)

module Hla : sig
  type direct = private Direct_tag
  type indirect = private Indirect_tag
  type _ operand

  val lit : int64 -> direct operand
  val reg : box -> direct operand
  val lab : label -> direct operand
  val ptr : direct operand -> indirect operand
  val off : box -> int64 -> indirect operand
  val push : _ operand -> Graph.fragment
  val assign : _ operand -> _ operand -> Graph.fragment
  val call : use:box list -> stack_count:int -> _ operand -> Graph.fragment
  val branch : label -> Graph.fragment

  val cbranch :
    rel -> _ operand -> _ operand -> label -> label -> Graph.fragment

  val arith :
    [`Add | `Subtract | `Multiply] -> _ operand -> _ operand -> Graph.fragment
end = struct
  type direct = private Direct_tag
  type indirect = private Indirect_tag

  type _ operand =
    | Lit of int64
    | Reg of box
    | Lab of label
    | Ptr of direct operand
    | Off of box * int64

  let lit c = Lit c
  let reg b = Reg b
  let lab l = Lab l
  let ptr op = Ptr op
  let off b x = Off (b, x)

  let pp_ptr pp_box ppf op =
    let fmt () = format_of_string "@[<h>QWORD@ PTR@ [%a]@]" in
    match op with
    | Lit x -> Fmt.pf ppf (fmt ()) Fmt.int64 x
    | Reg b -> Fmt.pf ppf (fmt ()) pp_box b
    | Lab l -> Fmt.pf ppf (fmt ()) pp_label l
    | _ -> assert false

  let pp_off pp_box ppf (b, x) =
    let sign, x =
      if Int64.compare x 0L < 0 then ("-", Int64.neg x) else ("+", x) in
    Fmt.pf ppf "@[<h>QWORD@ PTR@ [%a@ %s@ %a]@]" pp_box b sign Fmt.int64 x

  let push op =
    let fmt () = format_of_string "@[<h>push@ %a@]" in
    let s =
      match op with
      | Reg b ->
          Asm.v ~use:[b; sp] ~def:[sp] (fun ~use ~def:_ pp_box ppf () ->
              Fmt.pf ppf (fmt ()) pp_box (use 0) )
      | Lit x ->
          Asm.v ~use:[sp] ~def:[sp] (fun ~use:_ ~def:_ _ ppf () ->
              Fmt.pf ppf (fmt ()) Fmt.int64 x )
      | _ -> unimplemented "push" in
    Graph.asm s

  let assign dst src =
    let fmt () = format_of_string "@[<h>%s@ %a,@ %a@]" in
    let s =
      match (dst, src) with
      | Reg b1, Reg b2 ->
          Asm.v ~is_move:true ~use:[b2] ~def:[b1]
            (fun ~use ~def pp_box ppf () ->
              Fmt.pf ppf (fmt ()) "mov" pp_box (def 0) pp_box (use 0) )
      | Reg b, Lit x ->
          Asm.v ~def:[b] (fun ~use:_ ~def pp_box ppf () ->
              Fmt.pf ppf (fmt ()) "mov" pp_box (def 0) Fmt.int64 x )
      | Reg b, Lab l ->
          Asm.v ~def:[b] (fun ~use:_ ~def pp_box ppf () ->
              Fmt.pf ppf (fmt ()) "lea" pp_box (def 0) pp_label l )
      | Reg b, Ptr (Lab l) ->
          Asm.v ~def:[b] (fun ~use:_ ~def pp_box ppf () ->
              Fmt.pf ppf (fmt ()) "lea" pp_box (def 0) (pp_ptr pp_box) (Lab l) )
      | Reg b1, Ptr (Reg b2) ->
          Asm.v ~use:[b2] ~def:[b1] (fun ~use ~def pp_box ppf () ->
              Fmt.pf ppf (fmt ()) "mov" pp_box (def 0) (pp_ptr pp_box)
                (Reg (use 0)) )
      | Reg b1, Off (b2, x) ->
          Asm.v ~use:[b2] ~def:[b1] (fun ~use ~def pp_box ppf () ->
              Fmt.pf ppf (fmt ()) "mov" pp_box (def 0) (pp_off pp_box) (use 0, x) )
      | Ptr (Reg b1), Reg b2 ->
          Asm.v ~use:[b1; b2] (fun ~use ~def:_ pp_box ppf () ->
              Fmt.pf ppf (fmt ()) "mov" (pp_ptr pp_box)
                (Reg (use 0))
                pp_box (use 1) )
      | Ptr (Reg b), Lit x ->
          Asm.v ~def:[b] (fun ~use:_ ~def pp_box ppf () ->
              Fmt.pf ppf (fmt ()) "mov" (pp_ptr pp_box)
                (Reg (def 0))
                Fmt.int64 x )
      | Off (b1, x), Reg b2 ->
          Asm.v ~use:[b1; b2] (fun ~use ~def:_ pp_box ppf () ->
              Fmt.pf ppf (fmt ()) "mov" (pp_off pp_box)
                (use 0, x)
                pp_box (use 1) )
      | Off (b, x1), Lit x2 ->
          Asm.v ~use:[b] (fun ~use ~def:_ pp_box ppf () ->
              Fmt.pf ppf (fmt ()) "mov" (pp_off pp_box) (use 0, x1) Fmt.int64 x2 )
      | _, _ -> unimplemented "assign" in
    fun cursor -> Graph.asm s cursor

  let call_def = ax :: Iter.into_list Registers.trashed

  let call ~use ~stack_count x =
    let use = bp :: sp :: use in
    let def = call_def in
    let s =
      match x with
      | Lab l ->
          Asm.v ~use ~def (fun ~use:_ ~def:_ _ ppf () ->
              Fmt.pf ppf "@[<h>call@ %a@]" pp_label l )
      | Reg b ->
          Asm.v ~use:(b :: use) ~def (fun ~use ~def:_ pp_box ppf () ->
              Fmt.pf ppf "@[<h>call@ %a@]" pp_box (use 0) )
      | _ -> unimplemented "call" in
    let restore_stack =
      if stack_count <= 0 then Fun.id
      else fun f ->
        Graph.asm
          (Asm.v (fun ~use:_ ~def:_ pp_box ppf () ->
               Fmt.pf ppf "@[<h>add@ %a,@ %a@]" pp_box sp Fmt.int
                 (8 * stack_count) ) )
        @@ f in
    fun cursor -> Graph.asm s @@ restore_stack cursor

  let arith op lhs rhs =
    let opcode =
      match op with `Add -> "add" | `Subtract -> "sub" | `Multiply -> "imul"
    in
    let fmt () = format_of_string "@[<h>%s@ %a,@ %a@]" in
    let s =
      match (lhs, rhs) with
      | Reg b, Lit x ->
          Asm.v ~use:[b] ~def:[b] (fun ~use:_ ~def pp_box ppf () ->
              Fmt.pf ppf (fmt ()) opcode pp_box (def 0) Fmt.int64 x )
      | Reg b1, Reg b2 ->
          Asm.v ~use:[b1; b2] ~def:[b1] (fun ~use ~def pp_box ppf () ->
              Fmt.pf ppf (fmt ()) opcode pp_box (def 0) pp_box (use 1) )
      | _ -> unimplemented "arith" in
    Graph.asm s

  let branch l cursor = Graph.branch jump_asm l cursor

  let cbranch rel u v pos neg =
    let test =
      let fmt () = format_of_string "@[<h>cmp@ %a,@ %a@]" in
      match (u, v) with
      | Reg b1, Reg b2 ->
          Asm.v ~use:[b1; b2] (fun ~use ~def:_ pp_box ppf () ->
              Fmt.pf ppf (fmt ()) pp_box (use 0) pp_box (use 1) )
      | Reg b, Lit x ->
          Asm.v ~use:[b] (fun ~use ~def:_ pp_box ppf () ->
              Fmt.pf ppf (fmt ()) pp_box (use 0) Fmt.int64 x )
      | Ptr (Reg b), Lit x ->
          Asm.v ~use:[b] (fun ~use ~def:_ pp_box ppf () ->
              Fmt.pf ppf (fmt ()) (pp_ptr pp_box) (Reg (use 0)) Fmt.int64 x )
      | Off (b1, x), Reg b2 ->
          Asm.v ~use:[b1; b2] (fun ~use ~def:_ pp_box ppf () ->
              Fmt.pf ppf (fmt ()) (pp_off pp_box) (use 0, x) pp_box (use 1) )
      | Off (b, x1), Lit x2 ->
          Asm.v ~use:[b] (fun ~use ~def:_ pp_box ppf () ->
              Fmt.pf ppf (fmt ()) (pp_off pp_box) (use 0, x1) Fmt.int64 x2 )
      | _ -> unimplemented "cbranch" in
    let jump op l =
      Asm.v (fun ~use:_ ~def:_ _ ppf () ->
          Fmt.pf ppf "@[<h>%s@ %a@]" op pp_label l ) in
    let pair op1 op2 = (jump op1, jump op2) in
    let s, s_inv =
      match rel with
      | `Not_equal -> pair "jne" "je"
      | `Equal -> pair "je" "jne"
      | `Less -> pair "jl" "jge"
      | `Less_or_equal -> pair "jle" "jg"
      | `Greater -> pair "jg" "jle"
      | `Greater_or_equal -> pair "jge" "jl" in
    fun cursor -> Graph.asm test @@ Graph.cbranch s pos s_inv neg cursor
end

include Tree

let result f =
  let b = Fang_vm.box () in
  (f b, b)

let rec munch_value = function
  | Const c -> result (fun b -> Hla.(assign (reg b) (lit c)))
  | Loc l -> result (fun b -> Hla.(assign (reg b) (lab l)))
  | Box b -> (Fun.id, b)
  (* A value in an array with a constant offset. *)
  | Mem (Add (v1, Mul (Const x1, Add (Add (v2, Const x2), Const x3)))) ->
      result (fun b0 cursor ->
          let f1, b1 = munch_value v1 and f2, b2 = munch_value v2 in
          f1 @@ f2
          @@ Graph.asm
               (Asm.v ~use:[b1; b2] ~def:[b0] (fun ~use ~def pp_box ppf () ->
                    Fmt.pf ppf
                      "@[<h>mov@ %a,@ QWORD@ PTR@ [%a@ +@ %a*%a@ +@ %a]@]"
                      pp_box (def 0) pp_box (use 0) Fmt.int64 x1 pp_box (use 1)
                      Fmt.int64
                      Int64.(mul x1 (add x2 x3)) ) )
               cursor )
  | Mem (Add (v1, Mul (Const x1, Add (v2, Const x2)))) ->
      (* A value in an array. *)
      result (fun b0 cursor ->
          let f1, b1 = munch_value v1 and f2, b2 = munch_value v2 in
          f1 @@ f2
          @@ Graph.asm
               (Asm.v ~use:[b1; b2] ~def:[b0] (fun ~use ~def pp_box ppf () ->
                    Fmt.pf ppf
                      "@[<h>mov@ %a,@ QWORD@ PTR@ [%a@ +@ %a*%a@ +@ %a]@]"
                      pp_box (def 0) pp_box (use 0) Fmt.int64 x1 pp_box (use 1)
                      Fmt.int64 (Int64.mul x1 x2) ) )
               cursor )
  | Mem (Add (v, Const x)) ->
      result (fun b0 cursor ->
          let f, b = munch_value v in
          f @@ Hla.(assign (reg b0) (off b x)) cursor )
  | Mem (Sub (v, Const x)) ->
      result (fun b0 cursor ->
          let f, b = munch_value v in
          f @@ Hla.(assign (reg b0) (off b (Int64.neg x))) cursor )
  | Mem v ->
      result (fun b0 cursor ->
          let f, b = munch_value v in
          f @@ Hla.(assign (reg b0) (ptr (reg b))) cursor )
  | Perform (e, v) ->
      result (fun b0 cursor ->
          let f1 = munch_effect e in
          let f2, b2 = munch_value v in
          f1 @@ f2 @@ Hla.(assign (reg b0) (reg b2)) cursor )
  | Call (Loc l, vs) ->
      ( (fun cursor ->
          let f, use, stack_count = munch_args vs in
          f @@ Hla.(call ~use ~stack_count (lab l)) cursor )
      , ax )
  | Call (v, vs) ->
      ( (fun cursor ->
          let f1, b1 = munch_value v in
          let f2, use, stack_count = munch_args vs in
          f1 @@ f2 @@ Hla.(call ~use ~stack_count (reg b1)) cursor )
      , ax )
  | Add (v1, v2) -> munch_arith `Add v1 v2
  | Sub (v1, v2) -> munch_arith `Subtract v1 v2
  | Mul (v1, v2) -> munch_arith `Multiply v1 v2
  | Div (v1, v2) ->
      ( (fun cursor ->
          let f1, b1 = munch_value v1 in
          let f2, b2 = munch_value v2 in
          f1 @@ f2
          @@ Hla.(assign (reg dx) (lit 0L))
          @@ Hla.(assign (reg ax) (reg b1))
          @@ Graph.asm
               (Asm.v ~use:[ax; dx; b2] ~def:[ax; dx]
                  (fun ~use ~def:_ pp_box ppf () ->
                    Fmt.pf ppf "@[<h>idiv@ %a@]" pp_box (use 2) ) )
               cursor )
      , ax )
  | _ -> unimplemented "munch_value"

and munch_arith op v1 v2 =
  match (v1, v2) with
  | _, Const x2 ->
      result (fun b0 cursor ->
          let f, b = munch_value v1 in
          f
          @@ Hla.(assign (reg b0) (reg b))
          @@ Hla.(arith op (reg b0) (lit x2)) cursor )
  | _ ->
      result (fun b0 cursor ->
          let f1, b1 = munch_value v1 in
          let f2, b2 = munch_value v2 in
          f1 @@ f2
          @@ Hla.(assign (reg b0) (reg b1))
          @@ Hla.(arith op (reg b0) (reg b2)) cursor )

and munch_effect = function
  | Move (Box b1, Add (Box b2, Const 1L)) when Box.equal b1 b2 ->
      (* Increment. *)
      let b = b1 in
      fun cursor ->
        Graph.asm
          (Asm.v ~use:[b] ~def:[b] (fun ~use ~def:_ pp_box ppf () ->
               Fmt.pf ppf "@[<h>inc@ %a@]" pp_box (use 0) ) )
          cursor
  | Move
      ( Mem (Sub (v, Const x) as dst)
      , Add (Mem (Sub (_, Const _) as src), Const 1L) )
    when dst = src ->
      (* Increment a value at a negative memory offset. *)
      fun cursor ->
       let f, b = munch_value v in
       f
       @@ Graph.asm
            (Asm.v ~use:[b] ~def:[b] (fun ~use ~def:_ pp_box ppf () ->
                 Fmt.pf ppf "@[<h>inc@ QWORD@ PTR@ [%a@ -@ %a]@]" pp_box (use 0)
                   Fmt.int64 x ) )
            cursor
  | Move (Box b1, Sub (Box b2, Const 1L)) when Box.equal b1 b2 ->
      (* Decrement. *)
      let b = b1 in
      fun f ->
        Graph.asm
          (Asm.v ~use:[b] ~def:[b] (fun ~use ~def:_ pp_box ppf () ->
               Fmt.pf ppf "@[<h>dec@ %a@]" pp_box (use 0) ) )
        @@ f
  | Move (Box b, Const x) -> Hla.(assign (reg b) (lit x))
  | Move (Box b1, v) ->
      fun cursor ->
        let f, b = munch_value v in
        f @@ Hla.(assign (reg b1) (reg b)) cursor
  | Move
      ( Mem (Add (v1, Mul (Const x1, Add (Add (v2, Const x2), Const x3))))
      , Const x4 ) ->
      (* Assign a constant into an array at an index expression with constant
         offset. *)
      fun cursor ->
       let f1, b1 = munch_value v1 and f2, b2 = munch_value v2 in
       f1 @@ f2
       @@ Graph.asm
            (Asm.v ~use:[b1; b2] (fun ~use ~def:_ pp_box ppf () ->
                 Fmt.pf ppf "@[<h>mov@ QWORD@ PTR@ [%a@ +@ %a*%a@ +@ %a],@ %a@]"
                   pp_box (use 0) Fmt.int64 x1 pp_box (use 1) Fmt.int64
                   Int64.(mul x1 (add x2 x3))
                   Fmt.int64 x4 ) )
            cursor
  | Move (Mem (Add (v1, Mul (Const x1, Add (Add (v2, Const x2), Const x3)))), v3)
    ->
      (* Assign into an array at an index expression with constant offset. *)
      fun cursor ->
       let f1, b1 = munch_value v1
       and f2, b2 = munch_value v2
       and f3, b3 = munch_value v3 in
       f1 @@ f2 @@ f3
       @@ Graph.asm
            (Asm.v ~use:[b1; b2; b3] (fun ~use ~def:_ pp_box ppf () ->
                 Fmt.pf ppf "@[<h>mov@ QWORD@ PTR@ [%a@ +@ %a*%a@ +@ %a],@ %a@]"
                   pp_box (use 0) Fmt.int64 x1 pp_box (use 1) Fmt.int64
                   Int64.(mul x1 (add x2 x3))
                   pp_box (use 2) ) )
            cursor
  | Move (Mem (Add (v1, Mul (Const x1, Add (v2, Const x2)))), Const x3) ->
      (* Assign a constant into an array with an index expression. *)
      fun cursor ->
       let f1, b1 = munch_value v1 and f2, b2 = munch_value v2 in
       f1 @@ f2
       @@ Graph.asm
            (Asm.v ~use:[b1; b2] (fun ~use ~def:_ pp_box ppf () ->
                 Fmt.pf ppf "@[<h>mov@ QWORD@ PTR@ [%a@ +@ %a*%a@ +@ %a],@ %a@]"
                   pp_box (use 0) Fmt.int64 x1 pp_box (use 1) Fmt.int64
                   (Int64.mul x1 x2) Fmt.int64 x3 ) )
            cursor
  | Move (Mem (Add (v1, Mul (Const x1, Add (v2, Const x2)))), v3) ->
      (* Assign into an array with an index expression. *)
      fun cursor ->
       let f1, b1 = munch_value v1
       and f2, b2 = munch_value v2
       and f3, b3 = munch_value v3 in
       f1 @@ f2 @@ f3
       @@ Graph.asm
            (Asm.v ~use:[b1; b2; b3] (fun ~use ~def:_ pp_box ppf () ->
                 Fmt.pf ppf "@[<h>mov@ QWORD@ PTR@ [%a@ +@ %a*%a@ +@ %a],@ %a@]"
                   pp_box (use 0) Fmt.int64 x1 pp_box (use 1) Fmt.int64
                   (Int64.mul x1 x2) pp_box (use 2) ) )
            cursor
  | Move (Mem (Add (v, Const x1)), Const x2) ->
      (* Move a constant into a memory offset. *)
      fun cursor ->
       let f, b = munch_value v in
       f @@ Hla.(assign (off b x1) (lit x2)) cursor
  | Move (Mem (Sub (v, Const x1)), Const x2) ->
      (* Move a constant into a negative memory offset. *)
      fun cursor ->
       let f, b = munch_value v in
       f @@ Hla.(assign (off b (Int64.neg x1)) (lit x2)) cursor
  | Move (Mem (Add (v1, Const x)), v2) ->
      (* Move a value into a memory offset. *)
      fun cursor ->
       let f1, b1 = munch_value v1 in
       let f2, b2 = munch_value v2 in
       f1 @@ f2 @@ Hla.(assign (off b1 x) (reg b2)) cursor
  | Move (Mem (Sub (v1, Const x)), v2) ->
      (* Move a value into a negative memory offset. *)
      fun cursor ->
       let f1, b1 = munch_value v1 in
       let f2, b2 = munch_value v2 in
       f1 @@ f2 @@ Hla.(assign (off b1 (Int64.neg x)) (reg b2)) cursor
  | Move (Mem v, Const x) ->
      (* Move a constant into memory. *)
      fun cursor ->
       let f, b = munch_value v in
       f @@ Hla.(assign (ptr (reg b)) (lit x)) cursor
  | Move (Mem v1, v2) ->
      fun cursor ->
        let f1, b1 = munch_value v1 in
        let f2, b2 = munch_value v2 in
        f1 @@ f2 @@ Hla.(assign (ptr (reg b1)) (reg b2)) cursor
  | Discard v ->
      fun cursor ->
        let f, _ = munch_value v in
        f cursor
  | Seq (e1, e2) ->
      fun cursor ->
        let f1 = munch_effect e1 in
        let f2 = munch_effect e2 in
        f1 @@ f2 cursor
  | Cjump (r, Mem v, Const x, pos, neg) ->
      (* Upper-bound check for array indexing. *)
      fun cursor ->
       let f, b = munch_value v in
       f @@ Hla.(cbranch r (ptr (reg b)) (lit x) pos neg) cursor
  | Cjump (r, Mem (Sub (v1, Const x)), v2, pos, neg) ->
      (* Compare a negative memory offset to a value. *)
      fun cursor ->
       let f1, b1 = munch_value v1 in
       let f2, b2 = munch_value v2 in
       f1 @@ f2
       @@ Hla.(cbranch r (off b1 (Int64.neg x)) (reg b2) pos neg) cursor
  | Cjump (r, v, Const x, pos, neg) ->
      fun cursor ->
        let f, b = munch_value v in
        f @@ Hla.(cbranch r (reg b) (lit x) pos neg) cursor
  | Cjump (r, v1, v2, pos, neg) ->
      fun cursor ->
        let f1, b1 = munch_value v1 in
        let f2, b2 = munch_value v2 in
        f1 @@ f2 @@ Hla.(cbranch r (reg b1) (reg b2) pos neg) cursor
  | Jump l -> fun cursor -> Hla.(branch l) cursor
  | Def l -> fun cursor -> Graph.label l jump_asm cursor
  | _ -> assert false

and munch_args vs =
  let rec loop fr used_slots stack_count slots vs =
    match (slots, vs) with
    | _, [] -> (fr, used_slots, stack_count)
    | [], Const x :: vs ->
        (* A constant passed via the stack. *)
        loop
          (fun cursor -> Hla.(push (lit x)) @@ fr cursor)
          used_slots (stack_count + 1) [] vs
    | [], v :: vs ->
        (* A value passed via the stack. *)
        let f, b = munch_value v in
        loop
          (fun cursor -> f @@ Hla.(push (reg b)) @@ fr cursor)
          used_slots (stack_count + 1) [] vs
    | r :: slots, Const x :: vs ->
        loop
          (fun cursor -> fr @@ Hla.(assign (reg r) (lit x)) cursor)
          (r :: used_slots) stack_count slots vs
    | r :: slots, Loc l :: vs ->
        loop
          (fun cursor -> fr @@ Hla.(assign (reg r) (lab l)) cursor)
          (r :: used_slots) stack_count slots vs
    | r :: slots, Mem (Sub (Box b, Const x)) :: vs ->
        (* A value on the stack. *)
        loop
          (fun cursor -> fr @@ Hla.(assign (reg r) (off b (Int64.neg x))) cursor)
          (r :: used_slots) stack_count slots vs
    | r :: slots, v :: vs ->
        let f, b = munch_value v in
        loop
          (fun cursor -> fr @@ f @@ Hla.(assign (reg r) (reg b)) cursor)
          (r :: used_slots) stack_count slots vs in
  let f2, used_slots, stack_count =
    loop Fun.id [] 0 [di; si; dx; cx; r8; r9] vs in
  ((fun cursor -> f2 cursor), used_slots, stack_count)

let flow_graph l t =
  let f, b = munch_value (view t) in
  Graph.blur (f @@ Hla.(assign (reg ax) (reg b)) (Graph.entry (Graph.empty l)))
