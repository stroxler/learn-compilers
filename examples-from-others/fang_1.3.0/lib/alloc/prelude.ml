(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_asm
open Fang_flow
open Fang_iter
open Fang_vm

module type SPILLING = sig
  val preserve : box -> graph -> graph

  type handler

  val handler : box iter -> handler
  val stack_size_increase : handler -> int
  val was_spilled : box -> handler -> bool
  val save : stack_size:int -> box -> handler -> asm
  val restore : stack_size:int -> box -> handler -> asm
end
