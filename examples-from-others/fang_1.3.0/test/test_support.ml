(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_tiger

exception Unexpected_error of string
exception Unexpected_success

let make_lexbuf fragment = Lexing.from_string ~with_positions:true fragment

let parse_expecting_error =
  let module M = Parsing (Nop) in
  fun fragment ->
    match M.parse (make_lexbuf fragment) with
    | Error (e, _) -> e
    | Ok _ -> raise Unexpected_success

let stringify_error pp_error e =
  Fmt.strf "@[%a@]" Fmt.(const words (strf "%a" pp_error e)) ()

module Parse_and_validate (L : TIGER) = struct
  module M0 = Validation (L)
  module M1 = Parsing (M0)

  let apply fragment =
    match M1.parse (make_lexbuf fragment) with
    | Error (e, _) ->
        raise (Unexpected_error (stringify_error pp_parsing_error e))
    | Ok expr -> (
      match M0.validate expr with
      | Error (e, _) -> raise (Unexpected_error (stringify_error e ()))
      | Ok expr -> expr )
end

let parse_and_validate =
  let module M = Parse_and_validate (Nop) in
  M.apply

let parse_and_validate_expecting_error =
  let module M0 = Validation (Nop) in
  let module M1 = Parsing (M0) in
  fun fragment ->
    match M1.parse (make_lexbuf fragment) with
    | Error e -> `Parsing e
    | Ok expr -> (
      match M0.validate expr with
      | Error e -> `Validation e
      | Ok () -> raise Unexpected_success )

let wrap compile fragment =
  match compile (make_lexbuf fragment) with
  | Error e -> raise (Unexpected_error (stringify_error Fang.pp_error e))
  | Ok pp -> pp

let translate = wrap (Fang.compile_to_ir ~safe:true)
