(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_asm
open Fang_vm

val a : box
val b : box
val c : box
val d : box
val e : box
val pp_reg : box Fmt.t

type arg

val lit : int -> arg
val reg : box -> arg
val assign : box -> arg -> asm
val add : box -> arg -> arg -> asm
val sub : box -> arg -> arg -> asm
val mul : box -> arg -> arg -> asm
val jump : label -> asm
val test : box -> arg -> asm
val jeq : label -> asm
val jne : label -> asm
val jlt : label -> asm
val jle : label -> asm
val jgt : label -> asm
val jge : label -> asm
