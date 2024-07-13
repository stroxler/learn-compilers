(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

{
open Tokens

exception Invalid_string_character of char

exception Unterminated_string

exception Invalid_token of string
}

let digit = ['0'-'9']
let int = '-'? digit+
let space = [' ' '\t']+
let new_line = '\r' | '\n' | "\r\n"
let alpha = ['a'-'z' 'A'-'Z']
let identifier = alpha (alpha | digit | '_')*
let comment = "//" [^'\n' '\r']*

rule read = parse
  | space { read lexbuf }
  | comment { read lexbuf }
  | new_line { Lexing.new_line lexbuf; read lexbuf }
  | "and" { AND }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | "end" { END }
  | "break" { BREAK }
  | "let" { LET }
  | "in" { IN }
  | "var" { VAR }
  | "for" { FOR }
  | "to" { TO }
  | "array" { ARRAY }
  | "of" { OF }
  | "type" { TYPE }
  | "function" { FUNCTION }
  | "nil" { NIL }
  | "size" { SIZE }
  | '{' { BRACE_LEFT }
  | '}' { BRACE_RIGHT }
  | '(' { PAREN_LEFT }
  | ')' { PAREN_RIGHT }
  | '[' { BRACKET_LEFT }
  | ']' { BRACKET_RIGHT }
  | '+' { PLUS }
  | '-' { MINUS }
  | '&' { LOGICAL_AND }
  | '|' { LOGICAL_OR }
  | '*' { STAR }
  | '/' { FORWARD_SLASH }
  | '=' { EQUAL }
  | "<>" { NOT_EQUAL }
  | '<' { LESS }
  | "<=" { LESS_EQUAL }
  | '>' { GREATER }
  | ">=" { GREATER_EQUAL }
  | ":=" { ASSIGN }
  | '.' { DOT }
  | ',' { COMMA }
  | ':' { COLON }
  | ';' { SEMI }
  | '"' { read_string lexbuf.lex_start_p (Buffer.create 16) lexbuf }
  | int { INT (Int64.of_string (Lexing.lexeme lexbuf)) }
  | identifier { IDENTIFIER (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ { raise (Invalid_token (Lexing.lexeme lexbuf)) }

and read_string start buf =
  parse
  | '"' {
      let s = Buffer.contents buf in
      lexbuf.lex_start_p <- start;
      STRING s
  }
  | '\\' 'n' { Buffer.add_char buf '\n'; read_string start buf lexbuf }
  | '\\' 't' { Buffer.add_char buf '\t'; read_string start buf lexbuf }
  | '\\' '"' { Buffer.add_char buf '\"'; read_string start buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string start buf lexbuf }
  | [^ '"' '\\']+ {
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string start buf lexbuf
  }
  | eof { raise Unterminated_string }
  | _ as x { raise (Invalid_string_character x) }
