(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Prelude

type error =
  [ `Syntax_error
  | `Invalid_string_character of char
  | `Unterminated_string
  | `Invalid_token of string ]

let pp_error ppf = function
  | `Syntax_error -> Fmt.(box text) ppf "Syntax error"
  | `Invalid_string_character ch ->
      Fmt.(
        box
          ( const (quote char) ch
          ++ sp
          ++ const text "is an invalid character inside a string" ))
        ppf ()
  | `Unterminated_string -> Fmt.(box text) ppf "A string is unterminated"
  | `Invalid_token tok ->
      Fmt.(
        box (const (quote string) tok ++ sp ++ const text "is not a valid token"))
        ppf ()

module Make (L : TIGER) = struct
  module G = Grammar.Make (L)

  let error_location lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    Source_span.from_lexing_positions pos pos

  let parse lexbuf =
    try Ok (G.tiger Lexer.read lexbuf) with
    | G.Error -> Error (`Syntax_error, error_location lexbuf)
    | Lexer.Invalid_string_character ch ->
        Error (`Invalid_string_character ch, error_location lexbuf)
    | Lexer.Unterminated_string ->
        Error (`Unterminated_string, error_location lexbuf)
    | Lexer.Invalid_token tok ->
        Error (`Invalid_token tok, error_location lexbuf)
end
