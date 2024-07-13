(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

(** Register allocation.

    This implementation both coalesces boxes to minimize unnecessary move
    instructions and also handles spilling so that there's no limit to the
    number of local variables in a Tiger program.

    It's based on the "iterated register coallescing" algorithm \[1\].

    {1 References}

    \[1\] L. George and A. Appel, "Iterated register coalescing",
    {i ACM Transactions on Programming Languages and Systems}, vol. 18, no. 3,
    pp. 300-324, 1996. Available: 10.1145/229542.229546. *)

open Fang_asm
open Fang_flow
open Fang_iter
open Fang_vm

(** Necessary information for handling spills (i.e., boxes that cannot be
    assigned a register). *)
module type SPILLING = sig
  val preserve : box -> graph -> graph
  (** [preserve b g] is [g] amended so that [b]'s value after the amended graph
      is executed is always equal to its value beforehand, even if its value
      changes internally. *)

  (* State for handling spills. *)
  type handler

  val handler : box iter -> handler
  (** [handler bs] is a handler for the spilled registers [bs]. *)

  val stack_size_increase : handler -> int
  (** [stack_size_increase h] is the amount that the stack needs to grow to
      accomodate the spilled registers handled by [h]. *)

  val was_spilled : box -> handler -> bool
  (** [was_spilled b h] is [true] if [b] is a spilled register handled by [h]. *)

  val save : stack_size:int -> box -> handler -> asm
  (** [save ~stack_size b h] is an assembly instruction for saving [b] to the
      stack given a stack with total size [stack_size] (in byte). *)

  val restore : stack_size:int -> box -> handler -> asm
  (** [restore ~stack_size b h] is an assembly instruction for restoring [b]'s
      value from the stack given a stack with total size [stack_size] (in byte). *)
end

(** Allocating processor registers to temporary boxes. *)
module type ALLOC = sig
  val alloc_without_handling_spills :
    graph -> [`Allocation of graph | `Spill of Box.Set.t]
  (** [alloc_without_handling_spills g] is either a successful allocation of
      processor registers to temporary boxes or the set of boxes that could not
      be assigned a register (they were "spilled"). *)

  (** See {!SPILLING.handler}. *)
  type spilling_handler

  val adjust_for_spills : stack_size:int -> spilling_handler -> asm -> asm list
  (** [adjust_for_spills ~stack_size h s] is an expansion of an assembly
      instruction [s] into multiple instructions which account for the
      possibility that one of the read- or written-to boxes in [s] is spilled.

      For example, if an instruction needs to read from a box that has spilled,
      the instruction must be replaced with ones that first populates the box
      with the value from memory. *)

  val alloc : stack_size:int -> graph -> graph * int
  (** [alloc ~stack_size g] is a graph where every box has been assigned a
      processor register. [stack_size] is the size of the stack in the frame
      corresponding to [g].

      The result also includes the stack size of the amended graph. *)
end

module Alloc (R : sig
  val all : box iter
  val special : box iter
  val preserved : box iter
end)
(P : SPILLING) : ALLOC with type spilling_handler = P.handler
