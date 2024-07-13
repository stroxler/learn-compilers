(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_asm
open Fang_vm

let a = box ()
and b = box ()
and c = box ()
and d = box ()
and e = box ()

let pp_reg ppf r =
  if Box.compare r a = 0 then Fmt.string ppf "a"
  else if Box.compare r b = 0 then Fmt.string ppf "b"
  else if Box.compare r c = 0 then Fmt.string ppf "c"
  else if Box.compare r d = 0 then Fmt.string ppf "d"
  else if Box.compare r e = 0 then Fmt.string ppf "e"
  else Box.pp ppf r

type arg = Lit of int | Reg of box

let lit x = Lit x
let reg b = Reg b
let unary_fmt () = format_of_string "@[<h>%a@ <-@ %a@]"
let binary_fmt () = format_of_string "@[<h>%a@ <- %a@ %s@ %a@]"

let assign b1 = function
  | Lit x ->
      Asm.v ~def:[b1] (fun ~use:_ ~def pp_box ppf () ->
          Fmt.pf ppf (unary_fmt ()) pp_box (def 0) Fmt.int x )
  | Reg b2 ->
      Asm.v ~is_move:true ~def:[b1] ~use:[b2] (fun ~use ~def pp_box ppf () ->
          Fmt.pf ppf (unary_fmt ()) pp_box (def 0) pp_box (use 0) )

let arith op b1 v1 v2 =
  match (v1, v2) with
  | Reg b2, Lit x ->
      Asm.v ~use:[b2] ~def:[b1] (fun ~use ~def pp_box ppf () ->
          Fmt.pf ppf (binary_fmt ()) pp_box (def 0) pp_box (use 0) op Fmt.int x )
  | Reg b2, Reg b3 ->
      Asm.v ~use:[b2; b3] ~def:[b1] (fun ~use ~def pp_box ppf () ->
          Fmt.pf ppf (binary_fmt ()) pp_box (def 0) pp_box (use 0) op pp_box
            (use 1) )
  | _ -> assert false

let add = arith "+"
let sub = arith "-"
let mul = arith "*"

let jump l =
  Asm.v (fun ~use:_ ~def:_ _ ppf () ->
      Fmt.pf ppf "@[<h>jump@ %a@]" (Label.pp ()) l )

let test b1 v =
  let fmt () = format_of_string "@[<h>test@ %a,@ %a@]" in
  match v with
  | Lit x ->
      Asm.v ~use:[b1] (fun ~use ~def:_ pp_box ppf () ->
          Fmt.pf ppf (fmt ()) pp_box (use 0) Fmt.int x )
  | Reg b2 ->
      Asm.v ~use:[b1; b2] (fun ~use ~def:_ pp_box ppf () ->
          Fmt.pf ppf (fmt ()) pp_box (use 0) pp_box (use 1) )

let cond_jump inst l =
  Asm.v (fun ~use:_ ~def:_ _ ppf () ->
      Fmt.pf ppf "@[<h>%s@ %a@]" inst (Label.pp ()) l )

let jeq = cond_jump "jeq"
let jne = cond_jump "jne"
let jlt = cond_jump "jlt"
let jle = cond_jump "jle"
let jgt = cond_jump "jgt"
let jge = cond_jump "jge"
