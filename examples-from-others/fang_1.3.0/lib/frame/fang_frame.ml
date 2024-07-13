(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

(** Architecture-independent specification of stack frames. *)

open Fang_ir
open Fang_iter
open Fang_vm

(** Describes the scope of a value allocated in a stack frame. *)
type scope = [`Local | `Escapes]

(** Describes whether a frame requires an explicit frame pointer or not. *)
type frame_ptr =
  [ `No_frame_ptr
  | `Frame_ptr of int
    (** A frame pointer is required for this frame and this much space (in byte)
        is required for local variables. *) ]

module type FRAME = sig
  (** The state of a stack frame. *)
  type t

  (** A handle to a value allocated in a frame. *)
  type handle

  (** An IR fragment. *)
  type _ ir

  val word_size : int
  (** The size, in byte, of a machine word in this architecture. *)

  val address : value ir
  (** The address of a frame from inside the frame itself. *)

  val make : string -> scope list -> t
  (** Define a new frame with the given name and a list of frame parameter
      scopes. *)

  val args : t -> int -> handle
  (** The arguments provided to the frame by the caller. *)

  val label : t -> label
  (** The label corresponding to a frame. *)

  val name : t -> string
  (** The name of a frame. *)

  val alloc : scope -> t -> handle * t
  (** Allocate a value in the frame with the given scope. *)

  val access : address:value ir -> handle -> value ir
  (** [access ~address h] is the IR fragment for the frame value referred to by
      the handle given the address [address] of the frame in which the value was
      allocated. *)

  val call_external : string -> value ir list -> value ir
  (** Call the externally-defined function with the given name in an
      architecture-specific fashion. *)

  val finalize : value ir -> t -> value ir * frame_ptr
  (** Augment the body of a frame with necessary set-up and tear-down based on
      the current frame state. *)
end

(** Describes the calling-convention for a particular architecture. *)
module type REGISTERS = sig
  val all : box iter
  (** All registers. *)

  val special : box iter
  (** These registers are not available for general-purpose coloring. They serve
      particular functions, such as storing the stack pointer. *)

  val trashed : box iter
  (** These registers may be written to inside of a subroutine. *)

  val preserved : box iter
  (** From the perspective of the caller, these registers are never modified by
      a subroutine (the subroutine will save and restore them internally if they
      are necessary). *)
end
