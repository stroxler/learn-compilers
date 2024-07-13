(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_tiger

let%test_module _ =
  ( module struct
    (* So that we don't need to provide tags for all the fragments. *)
    module M = Test_support.Parse_and_validate (Pretty)

    let test fragment = Fmt.pr "%a" Pretty.pp (M.apply fragment)

    let%expect_test "int" = test {| 10 |} ; [%expect {| 10 |}]
    let%expect_test "string" = test {| "abc" |} ; [%expect {| "abc" |}]
    let%expect_test "nil" = test {| nil |} ; [%expect {| nil |}]

    let%test_module "arith" =
      ( module struct
        let%expect_test "add" = test {| 1 + 2 |} ; [%expect {| 1 + 2 |}]
        let%expect_test "subtract" = test {| 1 - 2 |} ; [%expect {| 1 - 2 |}]
        let%expect_test "multiply" = test {| 3 * 4 |} ; [%expect {| 3 * 4 |}]
        let%expect_test "divide" = test {| 10 / 3 |} ; [%expect {| 10 / 3 |}]
        let%expect_test "and" = test {| 2 & 3 |} ; [%expect {| 2 & 3 |}]
        let%expect_test "or" = test {| 3 | 5 |} ; [%expect {| 3 | 5 |}]
      end )

    let%expect_test "neg" = test {| -(1 + 2) |} ; [%expect {| -(1 + 2) |}]

    let%test_module "cmp" =
      ( module struct
        let%expect_test "equal" = test {| 1 = 2 |} ; [%expect {| 1 = 2 |}]
        let%expect_test "not_equal" = test {| 1 <> 2 |} ; [%expect {| 1 <> 2 |}]
        let%expect_test "less" = test {| 1 < 2 |} ; [%expect {| 1 < 2 |}]

        let%expect_test "less_or_equal" =
          test {| 1 <= 2 |} ; [%expect {| 1 <= 2 |}]

        let%expect_test "greater" = test {| 1 > 2 |} ; [%expect {| 1 > 2 |}]

        let%expect_test "greater_or_equal" =
          test {| 1 >= 2 |} ; [%expect {| 1 >= 2 |}]
      end )

    let%expect_test "value" =
      test {| let var x := 10 in x + 2 end |} ;
      [%expect {| let var x := 10 in x + 2 end |}]

    let%test_module "if" =
      ( module struct
        let%expect_test _ =
          test {| if 1 then print("AAA") |} ;
          [%expect {| if 1 then print("AAA") |}]

        let%expect_test _ =
          test {| if 1 then 10 else 20 |} ;
          [%expect {| if 1 then 10 else 20 |}]
      end )

    let%expect_test "while" =
      test {| while 1 do print("AAA") |} ;
      [%expect {| while 1 do print("AAA") |}]

    let%expect_test "for" =
      test {| for i := 1 to 10 do print_int(i) |} ;
      [%expect {| for i := 1 to 10 do print_int(i) |}]

    let%expect_test "break" =
      test {| while 1 do break |} ;
      [%expect {| while 1 do break |}]

    let%test_module "seq" =
      ( module struct
        let%expect_test _ = test {| (10) |} ; [%expect {| 10 |}]

        let%expect_test _ =
          test {| (1; 2 + 3 + 4; 5 + 6 + 7 + 8) |} ;
          [%expect {| (1; (2 + 3) + 4; ((5 + 6) + 7) + 8) |}]
      end )

    let%test_module "call" =
      ( module struct
        let%expect_test _ =
          test {| print_line() |} ; [%expect {| print_line() |}]

        let%expect_test _ =
          test {| concat("abc", "def") |} ;
          [%expect {| concat("abc", "def") |}]
      end )

    let%expect_test "uniform_array" =
      test {| let type numbers = array of int in numbers[10] of 0 end |} ;
      [%expect {| let type numbers = array of int in numbers[10] of 0 end |}]

    let%expect_test "array" =
      test
        {| let type numbers = array of int in numbers of [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] end |} ;
      [%expect
        {|
        let
          type numbers = array of int
        in
          numbers of [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        end |}]

    let%expect_test "size" =
      test {| let type numbers = array of int in size (numbers[10] of 0) end |} ;
      [%expect
        {| let type numbers = array of int in size numbers[10] of 0 end |}]

    let%expect_test "record" =
      test
        {| let type person = {name: string, age: int} in person {name="Joe", age=66} end |} ;
      [%expect
        {|
        let type person = {name: string,
                           age: int} in person {name="Joe", age=66} end |}]

    let%expect_test "scope" =
      test {| let var x := 10 var y := x + 1 var z := y + 1 in x * y * z end |} ;
      [%expect
        {|
        let var x := 10
            var y := x + 1
            var z := y + 1 in (x * y) * z end |}]

    let%expect_test "assign" =
      test {| let var x := 10 in x := x + 2 end |} ;
      [%expect {| let var x := 10 in x := x + 2 end |}]

    let%expect_test "index" =
      test
        {| let type numbers = array of int var xs := numbers[10] of 0 in xs[1] end |} ;
      [%expect
        {|
        let type numbers = array of int
            var xs := numbers[10] of 0 in xs[1] end |}]

    let%expect_test "access" =
      test
        {| let type person = {name: string, age: int} var p := person {name="Joe", age=66} in p.age end |} ;
      [%expect
        {|
        let
          type person = {name: string,
                         age: int}
          var p := person {name="Joe", age=66}
        in
          p.age
        end |}]

    let%test_module "var" =
      ( module struct
        let%expect_test _ =
          test {| let var x: int := 10 in end |} ;
          [%expect {| let var x: int := 10 in () end |}]

        let%expect_test _ =
          test {| let var x := 20 in end |} ;
          [%expect {| let var x := 20 in () end |}]
      end )

    let%expect_test "fns" =
      test
        {| let function is_even(n: int): int = if n = 0 then 1 else is_odd(n - 1) and function is_odd(n: int): int = if n = 0 then 0 else is_even(n - 1) in end |} ;
      [%expect
        {|
        let
          function is_even(n: int): int = if n = 0 then 1 else is_odd(n - 1)
          and function is_odd(n: int): int = if n = 0 then 0 else is_even(n - 1)
        in
          ()
        end |}]

    let%expect_test "types" =
      test {| let type score = int and type scores = array of score in end |} ;
      [%expect
        {| let type score = int and type scores = array of score in () end |}]

    let%test_module "fn" =
      ( module struct
        let%expect_test _ =
          test {| let function greet() = print("Hi!") in end |} ;
          [%expect {| let function greet() = print("Hi!") in () end |}]

        let%expect_test _ =
          test {| let function square(x: int): int = x * x in end |} ;
          [%expect {| let function square(x: int): int = x * x in () end |}]
      end )

    let%expect_test "alias_type" =
      test {| let type score = int in end |} ;
      [%expect {| let type score = int in () end |}]

    let%expect_test "array_type" =
      test {| let type numbers = array of int in end |} ;
      [%expect {| let type numbers = array of int in () end |}]

    let%expect_test "record_type" =
      test {| let type person = {name: string, age: int} in end |} ;
      [%expect
        {|
        let type person = {name: string,
                           age: int} in () end |}]
  end )
