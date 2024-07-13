(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_iter
open Fang_tiger

let%test_module _ =
  ( module struct
    module A = Analysis (Nop)
    module P = Test_support.Parse_and_validate (A)

    let test property pp fragment =
      let expr = P.apply fragment in
      let analysis, () = A.analyze expr in
      Fmt.pr "@[<v>%a@]"
        (Iter.pp Fmt.(Dump.pair Source_span.pp pp))
        (Property.iter (property analysis))

    let%expect_test "escaping" =
      test escaping
        (fun ppf -> function
          | `Local -> Fmt.string ppf "local"
          | `Escapes -> Fmt.string ppf "escapes" )
        {|
        let
          var x := 10
          var y := 20
          function add(z: int): int = z + x

          function magic_number(): int =
            let
              var x := x + 1
            in
              for i := 1 to 10 do
                let function foo(): int = i in (foo(); ()) end;
              100
            end
          in
            x; y; magic_number()
          end |} ;
      [%expect
        {|
        (3.10-3.21, escapes)
        (4.10-4.21, local)
        (5.23-5.24, local)
        (9.14-9.28, local)
        (11.14-12.62, escapes) |}]

    let%expect_test "leaves" =
      test leaves
        (fun ppf -> function
          | `Branch -> Fmt.string ppf "branch"
          | `Leaf -> Fmt.string ppf "leaf" )
        {|
        let
          function square(x: int): int = x * x

          function const(x: int): int =
            let
              function the_answer(): int = x
            in
              the_answer()
            end

          function recursive(x: int): int = recursive(x - 1)

          function useless(x: int): int =
            let
              function never(): int = x + 1
            in
              42
            end
        in
          square(3)
        end |} ;
      [%expect
        {|
        (3.10-3.46, leaf)
        (5.10-10.15, branch)
        (7.14-7.44, leaf)
        (12.10-12.60, leaf)
        (14.10-19.15, leaf)
        (16.14-16.43, leaf) |}]

    let%test_module "mutability" =
      ( module struct
        let test =
          test mutability (fun ppf -> function
            | `Constant -> Fmt.string ppf "constant"
            | `Mutates -> Fmt.string ppf "mutates" )

        let%expect_test _ =
          test
            {|
            let
              var x := 10
              var y := x + 2
              var z := y + 1
            in
              z := z - 1
            end |} ;
          [%expect
            {|
            (3.14-3.25, constant)
            (4.14-4.28, constant)
            (5.14-5.28, mutates) |}]

        let%expect_test _ =
          test
            {|
            let
              type cell = {value: int}
              var x := cell {value=10}
              var y := x
            in
              x := nil;
              print_int(y.value)
            end |} ;
          [%expect
            {|
            (4.14-4.38, mutates)
            (5.14-5.24, constant) |}]

        let%expect_test _ =
          test
            {|
            let
              function foo(x: int, y: int): int =
                (x := 10;
                 x)
            in
            end |} ;
          [%expect
            {|
            (3.27-3.28, mutates)
            (3.35-3.36, constant) |}]
      end )
  end )
