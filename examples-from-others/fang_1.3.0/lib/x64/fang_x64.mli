(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

(** Compiler functionality specialized to the x86-64 architecture. *)

open Fang_alloc
open Fang_flow
open Fang_frame
open Fang_ir
open Fang_translate
open Fang_vm

(** {1 Registers}

    For simplicity, Fang uses a uniform representation of a machine word for all
    values. Thus, while x86-64 registers are "segmented" into registers of
    different sizes (for example, [eax] is the 32 b segment of the [ax]
    register) we only ever use the 64 b variants (e.g, [rax]). *)

val ax : box
val bx : box
val cx : box
val dx : box
val si : box
val di : box
val sp : box
val bp : box
val r8 : box
val r9 : box
val r10 : box
val r11 : box
val r12 : box
val r13 : box
val r14 : box
val r15 : box

module Registers : REGISTERS

val pp_reg : box Fmt.t [@@ocaml.toplevel_printer]

(** {1 Translation to IR} *)

module Frame (I : IR) : FRAME with type 'a ir = 'a I.t
module Translate (I : IR) : TRANSLATE with type 'a ir = 'a I.t

(** {1 Emitting flow graphs} *)

module Emit : sig
  (** An emitter which producese a flow graph with assembly instructions for IR
      fragments. *)
  type _ t

  val flow_graph : label -> value t -> graph
  (** Produce a flow graph.

      [flow_graph lab e] is the flow graph produced by the emitter [e] for a
      subroutine with label [lab]. *)

  (** {1 Building emitters} *)

  include IR with type 'a t := 'a t  (** @closed *)
end

(** {1 Register allocation} *)

module Spilling : SPILLING
module Alloc : ALLOC with type spilling_handler = Spilling.handler

(** {1 Producing the final assembly}

    The assembly produced is compatible with the internal assemblers of both GCC
    and Clang. *)

val pp_asm_string : label -> string Fmt.t
(** Pretty-print assembly for a constant string with the given label. *)

val pp_asm_prefix : unit Fmt.t
(** This is the preamble necessary for a standalone assembly file. *)

val pp_asm : frame_ptr -> graph Fmt.t
(** Assuming the flow-graph has had processor registers assigned, produce the
    final assembly for the frame described by the graph. *)
