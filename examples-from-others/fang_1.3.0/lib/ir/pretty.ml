(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_vm

type _ t = box Fmt.t Fmt.t

let pp_label = Label.pp ()
let const x ppf _ = Fmt.int64 ppf x

let add v1 v2 ppf pp_box =
  Fmt.pf ppf "@[<hv 2>(+@ %a@ %a)@]" v1 pp_box v2 pp_box

let sub v1 v2 ppf pp_box =
  Fmt.pf ppf "@[<hv 2>(-@ %a@ %a)@]" v1 pp_box v2 pp_box

let mul v1 v2 ppf pp_box =
  Fmt.pf ppf "@[<hv 2>(*@ %a@ %a)@]" v1 pp_box v2 pp_box

let div v1 v2 ppf pp_box =
  Fmt.pf ppf "@[<hv 2>(/@ %a@ %a)@]" v1 pp_box v2 pp_box

let box b ppf pp_box = Fmt.box pp_box ppf b
let mem v ppf pp_box = Fmt.pf ppf "@[<hv 2>(mem@ %a)@]" v pp_box
let loc l ppf _ = Fmt.box pp_label ppf l

let perform e v ppf pp_box =
  Fmt.pf ppf "@[<hv 2>(perform@ %a@ %a)@]" e pp_box v pp_box

let call v vs ppf pp_box =
  match vs with
  | [] -> Fmt.pf ppf "@[<hv 2>(call@ %a)@]" v pp_box
  | _ ->
      let pps = List.map (fun v -> Fmt.(const v pp_box)) vs in
      Fmt.pf ppf "@[<hv 2>(call@ %a@ @[<hv>%a@])@]" v pp_box
        Fmt.(concat ~sep:sp pps)
        ()

let move v1 v2 ppf pp_box =
  Fmt.pf ppf "@[<hv 2>(move@ %a@ %a)@]" v1 pp_box v2 pp_box

let def l ppf _ = Fmt.pf ppf "@[<hv 2>(def@ %a)@]" pp_label l
let discard v ppf pp_box = Fmt.pf ppf "@[<hv 2>(discard@ %a)@]" v pp_box
let seq e1 e2 ppf pp_box = Fmt.pf ppf "@[<v>%a@ %a@]" e1 pp_box e2 pp_box

let pp_rel ppf = function
  | `Equal -> Fmt.string ppf "EQ"
  | `Not_equal -> Fmt.string ppf "NE"
  | `Less -> Fmt.string ppf "LT"
  | `Less_or_equal -> Fmt.string ppf "LE"
  | `Greater -> Fmt.string ppf "GT"
  | `Greater_or_equal -> Fmt.string ppf "GE"

let cjump rel v1 v2 l1 l2 ppf pp_box =
  Fmt.pf ppf "@[<hv 2>(cjump@ %a@ %a@ %a@ %a@ %a)@]" pp_rel rel v1 pp_box v2
    pp_box pp_label l1 pp_label l2

let jump l ppf _ = Fmt.pf ppf "@[<hv 2>(jump@ %a)@]" pp_label l
let pp pp_box ppf t = t ppf pp_box

let%test_module _ =
  ( module struct
    (* These tests are all for the "happy path" of no (or very few) breaks. *)

    let v0 = const 0L
    and v1 = const 1L
    and v2 = const 2L

    let e1 = discard v1
    let test t = Fmt.pr "%a@." (pp Box.pp) t

    let%expect_test "const" = test v0 ; [%expect {| 0 |}]

    let%expect_test "add" =
      test (add v1 v2) ;
      [%expect {| (+ 1 2) |}]

    let%expect_test "sub" =
      test (sub v1 v2) ;
      [%expect {| (- 1 2) |}]

    let%expect_test "mul" =
      test (mul v1 v2) ;
      [%expect {| (* 1 2) |}]

    let%expect_test "div" =
      test (div v1 v2) ;
      [%expect {| (/ 1 2) |}]

    let%expect_test "box" =
      test (box (Fang_vm.box ())) ;
      [%expect {| #0 |}]

    let%expect_test "mem" =
      test (mem v0) ;
      [%expect {| (mem 0) |}]

    let%expect_test "loc" =
      test (loc (global_label "foo")) ;
      [%expect {| foo |}]

    let%expect_test "perform" =
      test (perform e1 v2) ;
      [%expect {| (perform (discard 1) 2) |}]

    let%test_module "call" =
      ( module struct
        let%expect_test _ =
          test (call (loc (global_label "print")) []) ;
          [%expect {| (call print) |}]

        let%expect_test _ =
          test (call (loc (global_label "add")) [v1; v2]) ;
          [%expect {| (call add 1 2) |}]
      end )

    let%expect_test "move" =
      test (move (box (Fang_vm.box ())) v2) ;
      [%expect {| (move #1 2) |}]

    let%expect_test "def" =
      test (def (local_label ())) ;
      [%expect {| (def .L0) |}]

    let%expect_test "discard" =
      test (discard v0) ;
      [%expect {| (discard 0) |}]

    let%expect_test "seq" =
      test (seq (discard v1) (discard v2)) ;
      [%expect {|
        (discard 1)
        (discard 2) |}]

    let%expect_test "cjump" =
      test
        (cjump `Equal
           (box (Fang_vm.box ()))
           v2 (local_label ()) (local_label ()) ) ;
      [%expect {| (cjump EQ #2 2 .L2 .L1) |}]

    let%expect_test "jump" =
      let l = global_label "foo" in
      test (jump l) ;
      [%expect {| (jump foo) |}]
  end )
