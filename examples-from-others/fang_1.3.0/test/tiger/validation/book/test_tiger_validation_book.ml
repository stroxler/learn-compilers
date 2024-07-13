(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Test_tiger_validation_support

let%test_unit _ = test Snippets.n01
let%test_unit _ = test Snippets.n02
let%test_unit _ = test Snippets.n03
let%test_unit _ = test Snippets.n04
let%test_unit _ = test Snippets.n05
let%test_unit _ = test Snippets.n06
let%test_unit _ = test Snippets.n07
let%test_unit _ = test Snippets.n08

let%expect_test _ =
  test_error Snippets.n09 ;
  [%expect
    {| At 1.22-1.25: Expected a value of type int but this is a value of type string |}]

let%expect_test _ =
  test_error Snippets.n10 ;
  [%expect
    {| At 1.16-1.21: Expected a value of type unit but this is a value of type int |}]

let%expect_test _ =
  test_error Snippets.n11 ;
  [%expect
    {| At 1.15-1.18: Expected a value of type int but this is a value of type string |}]

let%test_unit _ = test Snippets.n12

let%expect_test _ =
  test_error Snippets.n13 ;
  [%expect
    {| At 1.0-1.8: Cannot compare these incompatible values of type int and string |}]

let%expect_test _ =
  test_error Snippets.n14 ;
  [%expect
    {| At 7.5-7.11: Cannot compare these incompatible values of type rec and arr |}]

let%expect_test _ =
  test_error Snippets.n15 ;
  [%expect
    {| At 1.11-1.12: Expected a value of type unit but this is a value of type int |}]

let%expect_test _ =
  test_error Snippets.n16 ;
  [%expect
    {|
    At 2.3-5.17: A cycle of mutually-recursive type definitions must pass through
                 either a record or array type |}]

let%expect_test _ =
  test_error Snippets.n17 ; [%expect {| At 2.2-2.40: Undefined type list |}]

let%expect_test _ =
  test_error Snippets.n18 ; [%expect {| At 3.5-3.14: Undefined function f2 |}]

let%expect_test _ =
  test_error Snippets.n19 ; [%expect {| At 5.8-5.9: Undefined variable a |}]

let%expect_test _ =
  test_error Snippets.n20 ; [%expect {| At 1.17-1.18: Undefined variable i |}]

let%expect_test _ =
  test_error Snippets.n21 ;
  [%expect
    {| At 3.29-3.45: Expected a value of type int but this is a value of type unit |}]

let%expect_test _ =
  test_error Snippets.n22 ;
  [%expect {| At 5.2-5.8: The record type rec has no field nam |}]

let%expect_test _ =
  test_error Snippets.n23 ;
  [%expect
    {| At 5.13-5.14: Expected a value of type string but this is a value of type int |}]

let%expect_test _ =
  test_error Snippets.n24 ;
  [%expect {| At 4.2-4.3: Cannot index a value of non-array type int |}]

let%expect_test _ =
  test_error Snippets.n25 ;
  [%expect
    {| At 4.2-4.3: Cannot access the field f of a value of non-record type int |}]

let%expect_test _ =
  test_error Snippets.n26 ;
  [%expect
    {| At 1.4-1.9: Expected a value of type int but this is a value of type string |}]

let%test_unit _ = test Snippets.n27

let%expect_test _ =
  test_error Snippets.n28 ;
  [%expect
    {| At 4.18-4.42: Expected a value of type rec1 but this is a value of type rec2 |}]

let%expect_test _ =
  test_error Snippets.n29 ;
  [%expect
    {| At 4.18-4.31: Expected a value of type arr1 but this is a value of type arr2 |}]

let%expect_test _ =
  test_error Snippets.n30 ;
  [%expect
    {| At 4.18-4.31: Expected a value of type arr1 but this is a value of type arr2 |}]

let%expect_test _ =
  test_error Snippets.n31 ;
  [%expect
    {| At 2.16-2.19: Expected a value of type int but this is a value of type string |}]

let%expect_test _ =
  test_error Snippets.n32 ;
  [%expect
    {| At 3.22-3.25: Expected a value of type int but this is a value of type string |}]

let%expect_test _ =
  test_error Snippets.n33 ;
  [%expect {| At 2.11-2.21: The type rectype is not defined |}]

let%expect_test _ =
  test_error Snippets.n34 ;
  [%expect
    {| At 4.4-4.9: Expected a value of type int but this is a value of type string |}]

let%expect_test _ =
  test_error Snippets.n35 ;
  [%expect
    {| At 4.2-4.10: The function g expects 2 arguments but has been invoked with 1 |}]

let%expect_test _ =
  test_error Snippets.n36 ;
  [%expect
    {| At 4.2-4.16: The function g expects 2 arguments but has been invoked with 3 |}]

let%test_unit _ = test Snippets.n37

let%expect_test _ =
  test_error Snippets.n38 ;
  [%expect
    {|
    At 3.6-3.21: The type a cannot be defined more than once in this group of
                 type definitions (it's already an alias defined at 2.2-2.14) |}]

let%expect_test _ =
  test_error Snippets.n39 ;
  [%expect
    {|
    At 2.2-3.33: The function g cannot be defined multiple times in this group of
                 mutually-recursive function definitions |}]

let%expect_test _ =
  test_error Snippets.n40 ;
  [%expect
    {| At 2.23-2.24: Expected a value of type unit but this is a value of type int |}]

let%test_unit _ = test Snippets.n41
let%test_unit _ = test Snippets.n42

let%expect_test _ =
  test_error Snippets.n43 ;
  [%expect
    {| At 4.2-4.3: Expected a value of type int but this is a value of type unit |}]

let%test_unit _ = test Snippets.n44

let%expect_test _ =
  test_error Snippets.n45 ;
  [%expect
    {|
    At 3.2-3.14: a cannot be assigned the value nil without its type being
                 constrained to a particular record type |}]

let%test_unit _ = test Snippets.n46
let%test_unit _ = test Snippets.n47
let%test_unit _ = test Snippets.n48

let%expect_test _ =
  test_error Snippets.n49 ; [%expect {| At 3.18: Syntax error |}]
