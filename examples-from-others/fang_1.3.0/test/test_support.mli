(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang
open Fang_tiger

exception Unexpected_error of string
exception Unexpected_success

val parse_expecting_error : string -> parsing_error

module Parse_and_validate (L : TIGER) : sig
  val apply : string -> expr L.t
end

val parse_and_validate : string -> unit
val parse_and_validate_expecting_error : string -> error
val translate : string -> unit Fmt.t
