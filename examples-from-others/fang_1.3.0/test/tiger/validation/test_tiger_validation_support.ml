(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

let test fragment = Test_support.parse_and_validate fragment

let test_error fragment =
  Fmt.pr "%a" Fang.pp_error
    (Test_support.parse_and_validate_expecting_error fragment)
