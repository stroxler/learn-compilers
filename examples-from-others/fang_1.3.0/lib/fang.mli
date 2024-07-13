(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

(** High-level compiler interface.

    While most of Fang's internals are architecture-agnostic, all the
    functionality here is specialized to the x86-64 architecture for
    convenience.

    Unlike the functionality in {!Fang_x64}, these functions compile Tiger
    "end-to-end": from parsing to producing output. *)

open Fang_flow
open Fang_frame
open Fang_ir
open Fang_iter
open Fang_tiger
open Fang_vm

(** {1 Compilation errors} *)

type error =
  [ `Parsing of parsing_error * source_span
  | `Validation of validation_error * source_span ]

val pp_error : error Fmt.t [@@ocaml.toplevel_printer]

(** {1 Compilation} *)

val compile_to_tiger : Lexing.lexbuf -> (unit Fmt.t, error) result
val compile_to_ir : ?safe:bool -> Lexing.lexbuf -> (unit Fmt.t, error) result

val compile_to_canonical_ir :
  ?safe:bool -> Lexing.lexbuf -> (unit Fmt.t, error) result

val compile_to_flow : ?safe:bool -> Lexing.lexbuf -> (unit Fmt.t, error) result

val compile_to_allocated_flow :
  ?safe:bool -> Lexing.lexbuf -> (unit Fmt.t, error) result

val compile_to_asm : ?safe:bool -> Lexing.lexbuf -> (unit Fmt.t, error) result

(** {1 Advanced interfaces}

    These interfaces are more flexible then the simple functions above, but most
    of the time that flexibility shouldn't be required. *)

module Compile_to_tiger (T : TIGER) : sig
  val apply : Lexing.lexbuf -> (expr T.t, error) result
end

module Compile_to_ir (I : IR) : sig
  val apply :
       ?safe:bool
    -> Lexing.lexbuf
    -> ( (label * (value I.t * frame_ptr)) iter * (label * string) iter
       , error )
       result
end

module Compile_to_canonical_ir (I : IR) : sig
  val apply :
       ?safe:bool
    -> Lexing.lexbuf
    -> ( (label * (value I.t * frame_ptr)) iter * (label * string) iter
       , error )
       result
end

module Compile_to_flow : sig
  val apply :
       ?safe:bool
    -> Lexing.lexbuf
    -> ((graph * frame_ptr) iter * (label * string) iter, error) result
end

module Compile_to_allocated_flow : sig
  val apply :
       ?safe:bool
    -> Lexing.lexbuf
    -> ((graph * frame_ptr) iter * (label * string) iter, error) result
end
