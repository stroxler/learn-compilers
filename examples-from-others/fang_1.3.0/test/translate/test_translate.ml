(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

let%test_module _ =
  ( module struct
    let test fragment = Test_support.translate fragment Fmt.stdout ()

    let%expect_test "int" = test {| 2 |} ; [%expect {| (frame fang no-fp 2) |}]

    let%expect_test "string" =
      test {| "abc" |} ;
      [%expect {| (frame fang no-fp str0) (string str0 "abc") |}]

    let%expect_test "nil" =
      test {| nil |} ; [%expect {| (frame fang no-fp 0) |}]

    let%test_module "arith" =
      ( module struct
        let%expect_test "add" =
          test {| 3 + 4 |} ; [%expect {| (frame fang no-fp (+ 3 4)) |}]

        let%expect_test "subtract" =
          test {| 3 - 4 |} ; [%expect {| (frame fang no-fp (- 3 4)) |}]

        let%expect_test "multiply" =
          test {| 3 * 4 |} ; [%expect {| (frame fang no-fp (* 3 4)) |}]

        let%expect_test "divide" =
          test {| 3 / 4 |} ; [%expect {| (frame fang no-fp (/ 3 4)) |}]

        let%expect_test "and" =
          test {| 3 & 4 |} ;
          [%expect
            {|
            (frame fang no-fp
              (perform
                (cjump NE 3 0 .L0 .L1)
                (def .L0)
                (move #16 4)
                (jump .L2)
                (def .L1)
                (move #16 0)
                (def .L2)
                #16)) |}]

        let%expect_test "or" =
          test {| 3 | 4 |} ;
          [%expect
            {|
            (frame fang no-fp
              (perform
                (cjump NE 3 0 .L3 .L4)
                (def .L3)
                (move #17 1)
                (jump .L5)
                (def .L4)
                (move #17 4)
                (def .L5)
                #17)) |}]
      end )

    let%expect_test "neg" =
      test {| -(-3) |} ; [%expect {| (frame fang no-fp (- 0 -3)) |}]

    let%test_module "cmp" =
      ( module struct
        let%test_module "[Strings]" =
          ( module struct
            let%expect_test "equal" =
              test {| "abc" = "def" |} ;
              [%expect
                {|
             (frame fang no-fp
               (perform
                 (move #18 1)
                 (cjump EQ (call fang_string_compare str0 str1) 0 .L6 .L7)
                 (def .L7)
                 (move #18 0)
                 (def .L6)
                 #18))
             (string str0 "abc")
             (string str1 "def") |}]

            let%expect_test "not_equal" =
              test {| "abc" <> "def" |} ;
              [%expect
                {|
                (frame fang no-fp
                  (perform
                    (move #19 1)
                    (cjump NE (call fang_string_compare str0 str1) 0 .L8 .L9)
                    (def .L9)
                    (move #19 0)
                    (def .L8)
                    #19))
                (string str0 "abc")
                (string str1 "def") |}]

            let%expect_test "less" =
              test {| "abc" < "def" |} ;
              [%expect
                {|
                (frame fang no-fp
                  (perform
                    (move #20 1)
                    (cjump LT (call fang_string_compare str0 str1) 0 .L10 .L11)
                    (def .L11)
                    (move #20 0)
                    (def .L10)
                    #20))
                (string str0 "abc")
                (string str1 "def") |}]

            let%expect_test "less_or_equal" =
              test {| "abc" <= "def" |} ;
              [%expect
                {|
                (frame fang no-fp
                  (perform
                    (move #21 1)
                    (cjump LE (call fang_string_compare str0 str1) 0 .L12 .L13)
                    (def .L13)
                    (move #21 0)
                    (def .L12)
                    #21))
                (string str0 "abc")
                (string str1 "def") |}]

            let%expect_test "greater" =
              test {| "abc" > "def" |} ;
              [%expect
                {|
                (frame fang no-fp
                  (perform
                    (move #22 1)
                    (cjump GT (call fang_string_compare str0 str1) 0 .L14 .L15)
                    (def .L15)
                    (move #22 0)
                    (def .L14)
                    #22))
                (string str0 "abc")
                (string str1 "def") |}]

            let%expect_test "greater_or_equal" =
              test {| "abc" >= "def" |} ;
              [%expect
                {|
                (frame fang no-fp
                  (perform
                    (move #23 1)
                    (cjump GE (call fang_string_compare str0 str1) 0 .L16 .L17)
                    (def .L17)
                    (move #23 0)
                    (def .L16)
                    #23))
                (string str0 "abc")
                (string str1 "def") |}]
          end )

        let%expect_test "equal" =
          test {| 3 = 4 |} ;
          [%expect
            {|
            (frame fang no-fp
              (perform
                (move #24 1)
                (cjump EQ 3 4 .L18 .L19)
                (def .L19)
                (move #24 0)
                (def .L18)
                #24)) |}]

        let%expect_test "not_equal" =
          test {| 3 <> 4 |} ;
          [%expect
            {|
            (frame fang no-fp
              (perform
                (move #25 1)
                (cjump NE 3 4 .L20 .L21)
                (def .L21)
                (move #25 0)
                (def .L20)
                #25)) |}]

        let%expect_test "less" =
          test {| 3 < 4 |} ;
          [%expect
            {|
            (frame fang no-fp
              (perform
                (move #26 1)
                (cjump LT 3 4 .L22 .L23)
                (def .L23)
                (move #26 0)
                (def .L22)
                #26)) |}]

        let%expect_test "less_or_equal" =
          test {| 3 <= 4 |} ;
          [%expect
            {|
            (frame fang no-fp
              (perform
                (move #27 1)
                (cjump LE 3 4 .L24 .L25)
                (def .L25)
                (move #27 0)
                (def .L24)
                #27)) |}]

        let%expect_test "greater" =
          test {| 3 > 4 |} ;
          [%expect
            {|
            (frame fang no-fp
              (perform
                (move #28 1)
                (cjump GT 3 4 .L26 .L27)
                (def .L27)
                (move #28 0)
                (def .L26)
                #28)) |}]

        let%expect_test "greater_or_equal" =
          test {| 3 >= 4 |} ;
          [%expect
            {|
            (frame fang no-fp
              (perform
                (move #29 1)
                (cjump GE 3 4 .L28 .L29)
                (def .L29)
                (move #29 0)
                (def .L28)
                #29)) |}]
      end )

    let%expect_test "value" =
      test
        {|
        let 
          var x := 4
        in
          x
        end |} ;
      [%expect {| (frame fang no-fp (perform (move #30 4) #30)) |}]

    let%test_module "if" =
      ( module struct
        let%expect_test _ =
          test {| if 1 then print_int(3) |} ;
          [%expect
            {|
            (frame fang no-fp
              (perform
                (cjump NE 1 0 .L30 .L31)
                (def .L30)
                (discard (perform (discard (call fang_io_print_int 3)) 0))
                (def .L31)
                0)) |}]

        let%expect_test _ =
          test {| if 1 then 10 else 20 |} ;
          [%expect
            {|
            (frame fang no-fp
              (perform
                (cjump NE 1 0 .L32 .L33)
                (def .L32)
                (move #31 10)
                (jump .L34)
                (def .L33)
                (move #31 20)
                (def .L34)
                #31)) |}]
      end )

    let%expect_test "while" =
      test {| while 1 do print_int(3) |} ;
      [%expect
        {|
        (frame fang no-fp
          (perform
            (def .L35)
            (cjump NE 1 0 .L36 .L37)
            (def .L36)
            (discard (perform (discard (call fang_io_print_int 3)) 0))
            (jump .L35)
            (def .L37)
            0)) |}]

    let%expect_test "for" =
      test {| for i := 1 to 10 do print("AAA") |} ;
      [%expect
        {|
        (frame fang no-fp
          (perform
            (move #32 1)
            (move #33 10)
            (cjump LE #32 #33 .L38 .L40)
            (def .L38)
            (discard (call fang_io_print str0))
            (cjump LT #32 #33 .L39 .L40)
            (def .L39)
            (move #32 (+ #32 1))
            (jump .L38)
            (def .L40)
            0))
        (string str0 "AAA") |}]

    let%expect_test "break" =
      test {| while 1 do break |} ;
      [%expect
        {|
        (frame fang no-fp
          (perform
            (def .L41)
            (cjump NE 1 0 .L42 .L43)
            (def .L42)
            (discard (perform (jump .L43) 0))
            (jump .L41)
            (def .L43)
            0)) |}]

    let%test_module "seq" =
      ( module struct
        let%expect_test "unit" =
          test {| () |} ;
          [%expect {| (frame fang no-fp (perform (discard 0) 0)) |}]

        let%expect_test _ =
          test {| (3) |} ; [%expect {| (frame fang no-fp 3) |}]

        let%expect_test _ =
          test {| (print_int(3); print_int(4); print_int(5)) |} ;
          [%expect
            {|
        (frame fang no-fp
          (perform
            (discard
              (perform
                (discard (call fang_io_print_int 3))
                (discard (call fang_io_print_int 4))
                (perform (discard (call fang_io_print_int 5)) 0)))
            0)) |}]
      end )

    let%test_module "call" =
      ( module struct
        let%expect_test "[Built-in]" =
          test {| print_int(3) |} ;
          [%expect
            {| (frame fang no-fp (perform (discard (call fang_io_print_int 3)) 0)) |}]

        let%expect_test _ =
          test
            {|
            let
              function add(x: int, y: int): int = x + y
            in
              add(2, 3)
            end |} ;
          [%expect
            {|
            (frame fang (fp 0) (perform (discard 0) (call fang.add rbp 2 3)))
            (frame fang.add no-fp
              (perform (move #36 rdx)
                       (move #35 rsi)
                       (move #34 rdi) (+ #35 #36))) |}]
      end )

    let%expect_test "uniform_array" =
      test
        {|
       let
         type numbers = array of int
       in
         numbers[4] of 10
       end |} ;
      [%expect
        {| (frame fang no-fp (perform (discard 0) (call fang_alloc_array 4 10))) |}]

    let%expect_test "array" =
      test
        {|
        let
          type numbers = array of int
        in
          numbers of [10, 20, 30]
        end |} ;
      [%expect
        {|
        (frame fang no-fp
          (perform
            (discard 0)
            (perform
              (move #37 (call fang_alloc_array 3 0))
              (move (mem (+ #37 (* 8 (+ 0 1)))) 10)
              (move (mem (+ #37 (* 8 (+ 1 1)))) 20)
              (move (mem (+ #37 (* 8 (+ 2 1)))) 30)
              #37))) |}]

    let%test_module "size" =
      ( module struct
        let%expect_test "[Statically known]" =
          test
            {|
            let
              type numbers = array of int
              var xs := numbers[4] of 10
            in
              size xs
            end |} ;
          [%expect
            {|
            (frame fang no-fp
              (perform (discard 0)
                       (move #38 (call fang_alloc_array 4 10)) 4)) |}]

        let%expect_test _ =
          test
            {|
            let
              type numbers = array of int
              var n := 3
            in
              n := 4;
              size (numbers[n] of 10)
            end |} ;
          [%expect
            {|
            (frame fang no-fp
              (perform
                (discard 0)
                (move #39 3)
                (perform (move #39 4) (mem (call fang_alloc_array #39 10))))) |}]
      end )

    let%expect_test "record" =
      test
        {|
        let
          type person = {name: string, age: int}
        in
          person {name="Joe", age=66}
        end |} ;
      [%expect
        {|
        (frame fang no-fp
          (perform
            (discard 0)
            (perform
              (move #40 (call fang_alloc_record 2))
              (move (mem (+ #40 (* 8 0))) 66)
              (move (mem (+ #40 (* 8 1))) str0)
              #40)))
        (string str0 "Joe") |}]

    let%test_module "scope" =
      ( module struct
        let%expect_test "[Escapes]" =
          test
            {|
            let
              var x := 10
              function foo(): int = x + 1
            in
              foo()
            end |} ;
          [%expect
            {|
            (frame fang (fp 8)
              (perform (move (mem (+ rbp -8)) 10)
                       (discard 0) (call fang.foo rbp)))
            (frame fang.foo no-fp (perform (move #41 rdi) (+ (mem (+ #41 -8)) 1))) |}]

        let%expect_test _ =
          test
            {|
            let
              var x := 10
              function foo(): int = 3
            in
              let
                var x := 20
                function foo(): int = 4
              in
                x + foo()
              end;
              x + foo()
            end |} ;
          [%expect
            {|
            (frame fang (fp 0)
              (perform
                (move #42 10)
                (discard 0)
                (perform
                  (discard
                    (perform (move #44 20)
                             (discard 0) (+ #44 (call fang.foo_1 rbp))))
                  (+ #42 (call fang.foo rbp)))))
            (frame fang.foo no-fp (perform (move #43 rdi) 3))
            (frame fang.foo_1 no-fp (perform (move #45 rdi) 4)) |}]
      end )

    let%expect_test "assign" =
      test
        {|
        let
          var x := 10
        in
          x := 20
        end |} ;
      [%expect
        {|
        (frame fang no-fp
          (perform (discard (perform (move #46 10) (perform (move #46 20) 0))) 0)) |}]

    let%test_module "index" =
      ( module struct
        let%expect_test "[Statically known to be safe]" =
          test
            {|
            let
              type numbers = array of int
              var xs := numbers[5] of 1
            in
              xs[2]
            end |} ;
          [%expect
            {|
            (frame fang no-fp
              (perform
                (discard 0)
                (move #47 (call fang_alloc_array 5 1))
                (mem (+ #47 (* 8 (+ 2 1)))))) |}]

        let%expect_test "[Statically known to be unsafe]" =
          test
            {|
            let
              type numbers = array of int
              var xs := numbers[5] of 1
            in
              xs[10]
            end |} ;
          [%expect
            {|
            (frame fang no-fp
              (perform
                (discard 0)
                (move #48 (call fang_alloc_array 5 1))
                (mem (call fang_check_array_index #48 10)))) |}]

        let%expect_test _ =
          test
            {|
            let
              type numbers = array of int
              var xs := numbers[5] of 1
            in
              xs := numbers[10] of 2;
              xs[3]
            end |} ;
          [%expect
            {|
            (frame fang no-fp
              (perform
                (discard 0)
                (move #49 (call fang_alloc_array 5 1))
                (perform
                  (move #49 (call fang_alloc_array 10 2))
                  (perform
                    (move #50 #49)
                    (move #51 3)
                    (discard (call fang_check_array_index #50 #51))
                    (mem (+ #50 (* 8 (+ #51 1)))))))) |}]
      end )

    let%test_module "access" =
      ( module struct
        let%expect_test "[Statically known to be nil]" =
          test
            {|
            let
              type person = {name: string, age: int}
              var p: person := nil
            in
              p.age
             end |} ;
          [%expect
            {|
            (frame fang no-fp
              (perform (discard 0)
                       (move #52 0) (mem (call fang_error_nil_access)))) |}]

        let%expect_test "[Statically known to be non-nil]" =
          test
            {|
            let
              type person = {name: string, age: int}
              var p := person {name="Joe", age=66}
            in
              p.age
            end |} ;
          [%expect
            {|
            (frame fang no-fp
              (perform
                (discard 0)
                (move
                  #55
                  (perform
                    (move #54 (call fang_alloc_record 2))
                    (move (mem (+ #54 (* 8 0))) 66)
                    (move (mem (+ #54 (* 8 1))) str0)
                    #54))
                (perform (move #56 #55) (mem (+ #56 (* 8 0))))))
            (string str0 "Joe") |}]

        let%expect_test _ =
          test
            {|
            let
              type person = {name: string, age: int}
              var p: person := nil
            in
              p := person {name="Joe", age=66};
              p.name
            end |} ;
          [%expect
            {|
            (frame fang no-fp
              (perform
                (discard 0)
                (move #57 0)
                (perform
                  (move
                    #57
                    (perform
                      (move #58 (call fang_alloc_record 2))
                      (move (mem (+ #58 (* 8 0))) 66)
                      (move (mem (+ #58 (* 8 1))) str0)
                      #58))
                  (perform
                    (move #59 #57)
                    (cjump EQ #59 0 .L45 .L44)
                    (def .L45)
                    (discard (call fang_error_nil_access))
                    (def .L44)
                    (mem (+ #59 (* 8 1)))))))
            (string str0 "Joe") |}]
      end )

    let%expect_test "var" =
      test {|
        let
          var x := 10
        in
        end |} ;
      [%expect
        {|
        (frame fang no-fp
          (perform (discard (perform (move #60 10) (perform (discard 0) 0))) 0)) |}]

    let%test_module "fns" =
      ( module struct
        let%expect_test "[Recursive]" =
          test
            {|
            let
              function factorial(n: int): int =
                if n <= 0 then 1
                else n * factorial(n - 1)
            in
            end |} ;
          [%expect
            {|
            (frame fang (fp 0)
              (perform (discard (perform (discard 0) (perform (discard 0) 0))) 0))
            (frame fang.factorial no-fp
              (perform
                (move #62 rsi)
                (move #61 rdi)
                (perform
                  (cjump LE #62 0 .L46 .L47)
                  (def .L46)
                  (move #63 1)
                  (jump .L48)
                  (def .L47)
                  (move #63 (* #62 (call fang.factorial #61 (- #62 1))))
                  (def .L48)
                  #63))) |}]

        let%expect_test "[Mutually-recursive]" =
          test
            {|
            let
              function is_even(n: int): int =
                if n = 0 then 1
                else is_odd(n - 1)
              and function is_odd(n: int): int =
                if n = 0 then 0 else is_even(n - 1)
            in
            end |} ;
          [%expect
            {|
            (frame fang (fp 0)
              (perform (discard (perform (discard 0) (perform (discard 0) 0))) 0))
            (frame fang.is_even no-fp
              (perform
                (move #65 rsi)
                (move #64 rdi)
                (perform
                  (cjump EQ #65 0 .L49 .L50)
                  (def .L49)
                  (move #67 1)
                  (jump .L51)
                  (def .L50)
                  (move #67 (call fang.is_odd #64 (- #65 1)))
                  (def .L51)
                  #67)))
            (frame fang.is_odd (fp 8)
              (perform
                (move #66 rsi)
                (move (mem (+ rbp -8)) rdi)
                (perform
                  (cjump EQ #66 0 .L52 .L53)
                  (def .L52)
                  (move #68 0)
                  (jump .L54)
                  (def .L53)
                  (move #68 (call fang.is_even (mem (+ rbp -8)) (- #66 1)))
                  (def .L54)
                  #68))) |}]

        let%expect_test _ =
          test
            {|
            let
              function foo(x: int): int = x + 1
              and function bar(y: int): int = y - 2
            in
            end |} ;
          [%expect
            {|
            (frame fang (fp 0)
              (perform (discard (perform (discard 0) (perform (discard 0) 0))) 0))
            (frame fang.bar no-fp (perform (move #72 rsi)
                                           (move #71 rdi) (- #72 2)))
            (frame fang.foo no-fp (perform (move #70 rsi)
                                           (move #69 rdi) (+ #70 1))) |}]
      end )

    let%test_module "types" =
      ( module struct
        let%expect_test "[Recursive]" =
          test
            {|
            let
              type list = {head: int, tail: list}
            in
              list {head=1, tail=list {head=2, tail=list {head=3, tail=nil}}}
            end |} ;
          [%expect
            {|
            (frame fang no-fp
              (perform
                (discard 0)
                (perform
                  (move #73 (call fang_alloc_record 2))
                  (move (mem (+ #73 (* 8 0))) 1)
                  (move
                    (mem (+ #73 (* 8 1)))
                    (perform
                      (move #74 (call fang_alloc_record 2))
                      (move (mem (+ #74 (* 8 0))) 2)
                      (move
                        (mem (+ #74 (* 8 1)))
                        (perform
                          (move #75 (call fang_alloc_record 2))
                          (move (mem (+ #75 (* 8 0))) 3)
                          (move (mem (+ #75 (* 8 1))) 0)
                          #75))
                      #74))
                  #73))) |}]

        let%expect_test "[Mutually-recursive]" =
          test
            {|
            let
              type tree = {data: int, children: forest}
              and type forest = {head: tree, tail: forest}
            in
              tree {data=5, children=forest {head=tree {data=3, children=nil}, tail=nil}}
            end |} ;
          [%expect
            {|
            (frame fang no-fp
              (perform
                (discard 0)
                (perform
                  (move #76 (call fang_alloc_record 2))
                  (move
                    (mem (+ #76 (* 8 0)))
                    (perform
                      (move #77 (call fang_alloc_record 2))
                      (move
                        (mem (+ #77 (* 8 0)))
                        (perform
                          (move #78 (call fang_alloc_record 2))
                          (move (mem (+ #78 (* 8 0))) 0)
                          (move (mem (+ #78 (* 8 1))) 3)
                          #78))
                      (move (mem (+ #77 (* 8 1))) 0)
                      #77))
                  (move (mem (+ #76 (* 8 1))) 5)
                  #76))) |}]

        let%expect_test _ =
          test
            {|
            let
              type numbers = array of int
              and type person = {name: string, age: int}
            in
            end |} ;
          [%expect
            {|
            (frame fang no-fp
              (perform (discard (perform (discard 0) (perform (discard 0) 0))) 0)) |}]
      end )

    let%test_module "fn" =
      ( module struct
        let%expect_test "[Procedure]" =
          test
            {|
            let
              function do_it() = print_int(3)
            in
            end |} ;
          [%expect
            {|
            (frame fang (fp 0)
              (perform (discard (perform (discard 0) (perform (discard 0) 0))) 0))
            (frame fang.do_it no-fp
              (perform (move #79 rdi) (perform (discard (call fang_io_print_int 3)) 0))) |}]

        let%expect_test "[Non-leaf]" =
          test
            {|
            let
              function square(x: int): int = x * x
              function sum_of_squares(x: int, y: int): int = square(x) + square(y)
            in
              sum_of_squares(2, 3)
            end |} ;
          [%expect
            {|
            (frame fang (fp 0)
              (perform (discard 0)
                       (discard 0) (call fang.sum_of_squares rbp 2 3)))
            (frame fang.square no-fp (perform (move #81 rsi)
                                              (move #80 rdi) (* #81 #81)))
            (frame fang.sum_of_squares (fp 8)
              (perform
                (move #83 rdx)
                (move #82 rsi)
                (move (mem (+ rbp -8)) rdi)
                (+
                  (call fang.square (mem (+ rbp -8)) #82)
                  (call fang.square (mem (+ rbp -8)) #83)))) |}]

        let%expect_test "[Function]" =
          test
            {| 
            let
              function add(x: int, y: int): int = x + y
            in
            end |} ;
          [%expect
            {|
            (frame fang (fp 0)
              (perform (discard (perform (discard 0) (perform (discard 0) 0))) 0))
            (frame fang.add no-fp
              (perform (move #86 rdx)
                       (move #85 rsi)
                       (move #84 rdi) (+ #85 #86))) |}]
      end )

    let%expect_test "alias_type" =
      test
        {|
        let
          type name = string
          var p: name := "abc"
        in
        end |} ;
      [%expect
        {|
        (frame fang no-fp
          (perform
            (discard (perform (discard 0)
                              (move #87 str0) (perform (discard 0) 0)))
            0))
        (string str0 "abc") |}]

    let%expect_test "array_type" =
      test
        {|
        let
          type numbers = array of int
        in
        end |} ;
      [%expect
        {|
        (frame fang no-fp
          (perform (discard (perform (discard 0) (perform (discard 0) 0))) 0)) |}]

    let%expect_test "record_type" =
      test
        {|
        let
          type person = {name: string, age: int}
        in
        end |} ;
      [%expect
        {|
        (frame fang no-fp
          (perform (discard (perform (discard 0) (perform (discard 0) 0))) 0)) |}]
  end )
