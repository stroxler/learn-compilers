(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Test_tiger_validation_support

let%test_module "arith" =
  ( module struct
    let%expect_test _ =
      test_error Snippets.arith_bad_lhs_type ;
      [%expect
        {| At 1.0-1.5: Expected a value of type int but this is a value of type string |}]

    let%expect_test _ =
      test_error Snippets.arith_bad_rhs_type ;
      [%expect
        {| At 1.5-1.10: Expected a value of type int but this is a value of type string |}]
  end )

let%expect_test "neg" =
  test_error Snippets.neg_bad_type ;
  [%expect
    {| At 1.1-1.6: Expected a value of type int but this is a value of type string |}]

let%test_module "cmp" =
  ( module struct
    let%expect_test _ =
      test_error Snippets.cmp_type_mismatch ;
      [%expect
        {| At 1.0-1.10: Cannot compare these incompatible values of type int and string |}]

    let%expect_test _ =
      test_error Snippets.cmp_record_mag ;
      [%expect
        {|
    At 4.2-4.38: Cannot compare these record values. Record values can only be
                 compared for equality |}]

    let%expect_test _ =
      test_error Snippets.cmp_array_mag ;
      [%expect
        {|
    At 4.2-4.37: Cannot compare these array values. Array values can only be
                 compared for equality |}]

    let%expect_test _ =
      test_error Snippets.cmp_unit_mag ;
      [%expect
        {|
    At 1.0-1.8: Cannot compare these unit values. Unit values can only be
                compared for equality |}]
  end )

let%test_module "if" =
  ( module struct
    let%expect_test _ =
      test_error Snippets.if_bad_condition_type ;
      [%expect
        {| At 1.3-1.8: Expected a value of type int but this is a value of type string |}]

    let%expect_test _ =
      test_error Snippets.if_bad_body_type ;
      [%expect
        {|
      At 1.11-1.16: Expected a value of type unit but this is a value of type
                    string |}]

    let%expect_test _ =
      test_error Snippets.if_type_mismatch ;
      [%expect
        {| At 1.22-1.24: Expected a value of type string but this is a value of type int |}]
  end )

let%test_module "while" =
  ( module struct
    let%expect_test _ =
      test_error Snippets.while_bad_condition_type ;
      [%expect
        {| At 1.6-1.11: Expected a value of type int but this is a value of type string |}]

    let%expect_test _ =
      test_error Snippets.while_bad_body_type ;
      [%expect
        {| At 1.12-1.14: Expected a value of type unit but this is a value of type int |}]
  end )

let%test_module "for" =
  ( module struct
    let%expect_test _ =
      test_error Snippets.for_bad_initial_type ;
      [%expect
        {| At 1.9-1.14: Expected a value of type int but this is a value of type string |}]

    let%expect_test _ =
      test_error Snippets.for_bad_final_type ;
      [%expect
        {| At 1.14-1.19: Expected a value of type int but this is a value of type string |}]

    let%expect_test _ =
      test_error Snippets.for_bad_body_type ;
      [%expect
        {|
        At 1.20-1.25: Expected a value of type unit but this is a value of type
                      string |}]
  end )

let%expect_test "break" =
  test_error Snippets.break_outside_loop ;
  [%expect {| At 1.0-1.5: "break" cannot appear outside of looping contexts |}]

let%test_module "call" =
  ( module struct
    let%expect_test _ =
      test_error Snippets.call_undefined_function ;
      [%expect {| At 1.0-1.12: Undefined function foobar |}]

    let%expect_test _ =
      test_error Snippets.call_missing_args ;
      [%expect
        {|
        At 1.0-1.7: The function print expects 1 argument but has been invoked with 0 |}]

    let%expect_test _ =
      test_error Snippets.call_extra_args ;
      [%expect
        {|
        At 1.0-1.27: The function concat expects 2 arguments but has been invoked
                     with 3 |}]
  end )

let%test_module "uniform_array" =
  ( module struct
    let%expect_test _ =
      test_error Snippets.uniform_array_undefined_type ;
      [%expect {| At 1.0-1.16: The type numbers is not defined |}]

    let%expect_test _ =
      test_error Snippets.uniform_array_not_an_array ;
      [%expect
        {|
        At 1.0-1.12: Cannot create an array value of type int since that type is not
                     defined as an array (it's the built-in type int) |}]

    let%expect_test _ =
      test_error Snippets.uniform_array_bad_size_type ;
      [%expect
        {| At 4.10-4.15: Expected a value of type int but this is a value of type string |}]

    let%expect_test _ =
      test_error Snippets.uniform_array_bad_value_type ;
      [%expect
        {| At 4.17-4.22: Expected a value of type int but this is a value of type string |}]
  end )

let%test_module "array" =
  ( module struct
    let%expect_test _ =
      test_error Snippets.array_undefined_type ;
      [%expect {| At 1.0-1.16: The type foo is not defined |}]

    let%expect_test _ =
      test_error Snippets.array_not_an_array ;
      [%expect
        {|
        At 1.0-1.19: Cannot create an array value of type string since that type is
                     not defined as an array (it's the built-in type string) |}]

    let%expect_test _ =
      test_error Snippets.array_type_mismatch ;
      [%expect
        {| At 4.14-4.17: Expected a value of type int but this is a value of type string |}]
  end )

let%expect_test _ =
  test_error Snippets.fn_dup_params ;
  [%expect
    {|
    At 2.23-2.24: There cannot be multiple parameters with the name x in the
                  definition of add |}]

let%expect_test "size" =
  test_error Snippets.size_not_an_array ;
  [%expect
    {| At 4.11-4.12: Cannot compute the size of this non-array value of type int |}]

let%test_module "record" =
  ( module struct
    let%expect_test _ =
      test_error Snippets.record_undefined_type ;
      [%expect {| At 1.0-1.24: The type foo is not defined |}]

    let%expect_test _ =
      test_error Snippets.record_not_a_record ;
      [%expect
        {|
        At 4.2-4.30: Cannot create a record value of type numbers since that type is
                     not defined as a record (it's an array defined at 2.2-2.29) |}]

    let%expect_test _ =
      test_error Snippets.record_field_assigned_multiple_times ;
      [%expect
        {| At 4.2-4.37: Cannot assign a value to the record field age multiple times |}]

    let%expect_test _ =
      test_error Snippets.record_not_a_field ;
      [%expect
        {|
        At 4.2-4.33: Cannot assign values to these fields of the record type person
                     since they don't exist: nickname |}]

    let%expect_test _ =
      test_error Snippets.record_missing_field ;
      [%expect
        {|
        At 4.2-4.21: Missing assignments to these fields of the record type person:
                     age |}]
  end )

let%expect_test "assign" =
  test_error Snippets.assign_type_mismatch ;
  [%expect
    {| At 4.7-4.12: Expected a value of type int but this is a value of type string |}]

let%expect_test "name" =
  test_error Snippets.name_undefined ;
  [%expect {| At 1.0-1.3: Undefined variable foo |}]

let%test_module "index" =
  ( module struct
    let%expect_test _ =
      test_error Snippets.index_not_an_array ;
      [%expect {| At 4.2-4.3: Cannot index a value of non-array type int |}]

    let%expect_test _ =
      test_error Snippets.index_bad_index_type ;
      [%expect
        {| At 5.5-5.10: Expected a value of type int but this is a value of type string |}]
  end )

let%test_module "access" =
  ( module struct
    let%expect_test _ =
      test_error Snippets.access_not_a_record ;
      [%expect
        {| At 4.2-4.3: Cannot access the field age of a value of non-record type int |}]

    let%expect_test _ =
      test_error Snippets.access_not_a_field ;
      [%expect {| At 5.2-5.12: The record type person has no field nickname |}]
  end )

let%test_module "var" =
  ( module struct
    let%expect_test _ =
      test_error Snippets.var_unconstrained_nil ;
      [%expect
        {|
        At 2.2-2.14: p cannot be assigned the value nil without its type being
                     constrained to a particular record type |}]

    let%expect_test _ =
      test_error Snippets.var_undefined_type ;
      [%expect {| At 2.2-2.18: The type foo is not defined |}]

    let%expect_test _ =
      test_error Snippets.var_type_mismatch ;
      [%expect
        {| At 2.16-2.21: Expected a value of type int but this is a value of type string |}]
  end )

let%test_module "fns" =
  ( module struct
    let%expect_test _ =
      test_error Snippets.fns_multiple_definitions ;
      [%expect
        {|
        At 2.2-3.30: The function foo cannot be defined multiple times in this group
                     of mutually-recursive function definitions |}]

    let%expect_test _ =
      test_error Snippets.fns_type_mismatch ;
      [%expect
        {| At 2.24-2.29: Expected a value of type int but this is a value of type string |}]
  end )

let%test_module "types" =
  ( module struct
    let%expect_test _ =
      test_error Snippets.types_undefined_type_alias ;
      [%expect {| At 2.2-2.31: Undefined type float |}]

    let%expect_test _ =
      test_error Snippets.types_undefined_type_array ;
      [%expect {| At 2.2-2.29: Undefined type foo |}]

    let%expect_test _ =
      test_error Snippets.types_undefined_type_record ;
      [%expect {| At 2.2-2.37: Undefined type foo |}]

    let%expect_test _ =
      test_error Snippets.types_illegal_cycle ;
      [%expect
        {|
        At 2.2-4.16: A cycle of mutually-recursive type definitions must pass through
                     either a record or array type |}]
  end )

let%test_module "case" =
  ( module struct
    let%expect_test _ =
      test_error Snippets.fn_undefined_param_type ;
      [%expect {| At 2.2-2.45: The type float is not defined |}]

    let%expect_test _ =
      test_error Snippets.fn_undefined_return_type ;
      [%expect {| At 2.2-2.43: The type foo is not defined |}]

    let%expect_test _ =
      test_error Snippets.fn_duplicate_param_names ;
      [%expect
        {|
        At 2.23-2.24: There cannot be multiple parameters with the name x in the
                      definition of add |}]
  end )

let%expect_test "record_type" =
  test_error Snippets.record_type_duplicate_field_names ;
  [%expect
    {|
    At 2.2-2.54: There cannot be multiple fields with the name name in the
                 definition of the person record |}]
