(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_tiger

let%test_module "parsing" =
  ( module struct
    let test fragment =
      Fmt.pr "%a" pp_parsing_error (Test_support.parse_expecting_error fragment)

    let%expect_test "[An invalid Tiger fragment raises an error]" =
      test {| let x := 10 in x end |} ;
      [%expect {| Syntax error |}]

    let%expect_test "[An unterminated string raises an error]" =
      test {| "Hi there in s end |} ;
      [%expect {| A string is unterminated |}]

    let%expect_test "[An invalid token raises an error]" =
      test {| let var x := 2^3 in x |} ;
      [%expect {|
        "^" is not a valid token |}]

    let%expect_test "[An invalid string character raises an error]" =
      test {| "A newline.\nSomething\weird." |} ;
      [%expect {| "\" is an invalid character inside a string |}]
  end )
