(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

(** Translating Tiger to the intermediate representation (IR).

    Translation requires:

    - An architecture-specific specification of a stack frame (i.e., an
      implementation of {!Fang_frame.FRAME})
    - An interpreter for the IR (i.e., an implementation of {!Fang_ir.IR})

    A subsequent compiler phase converts this IR to a much nicer (and shorter!)
    canonical form. *)

open Fang_frame
open Fang_ir
open Fang_iter
open Fang_tiger
open Fang_vm

(** A fragment is any labelled thing. *)
type 'a fragment = label * 'a

val pp_string_fragment : string fragment Fmt.t
(** Pretty-print (for human consumption) a labeled string constant. *)

val pp_frame_fragment :
  box Fmt.t -> (value Fang_ir.Pretty.t * frame_ptr) fragment Fmt.t
(** Pretty-print (for human consumption) a labeled subroutine consisting of
    pretty-printed IR and the requirements for the frame pointer. *)

module type TRANSLATE = sig
  (** A translator from Tiger to IR. *)
  type _ t

  (** An IR interpreter. *)
  type _ ir

  (** {1 Executing translations} *)

  val translate :
       ?safe:bool
    -> analysis
    -> expr t
    -> (value ir * frame_ptr) fragment iter * string fragment iter
  (** Translate a Tiger value into IR.

      If [safe] is [false] (it's default value is [true]) then potentially
      memory-unsafe operations will be permitted: accessing a [nil] record value
      and accessing an array outside of its bounds. Unsafe code is faster.

      The result is a pair of iterators.

      The first iterates over all the subroutines necessary for implementing the
      Tiger fragment and the frame pointer requirement for each.

      The second iterates over all necessary string constants. *)

  (** {1 Building translators} *)

  include TIGER with type 'a t := 'a t  (** @closed *)
end

module Translate (F : FRAME) (I : IR with type 'a t = 'a F.ir) :
  TRANSLATE with type 'a ir = 'a I.t
