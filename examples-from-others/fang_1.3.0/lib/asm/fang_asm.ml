(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_iter
open Fang_vm

module Asm = struct
  exception Invalid_index of int

  type selector = int -> box
  type formatter = use:selector -> def:selector -> box Fmt.t -> unit Fmt.t

  type t =
    { id: int
    ; use: box array
    ; def: box array
    ; is_move: bool
    ; formatter: formatter }

  let next_id = ref 0

  let generate_id () =
    let id = !next_id in
    incr next_id ; id

  let v ?(is_move = false) ?(use = []) ?(def = []) formatter =
    { id= generate_id ()
    ; is_move
    ; use= Array.of_list use
    ; def= Array.of_list def
    ; formatter }

  let compare t1 t2 = Int.compare t1.id t2.id
  let use t = Iter.adapt Array.iter t.use
  let def t = Iter.adapt Array.iter t.def

  let modify_use f t =
    let use = Array.copy t.use in
    f use ; {t with use}

  let modify_def f t =
    let def = Array.copy t.def in
    f def ; {t with def}

  let map f t =
    let use = Array.map f t.use and def = Array.map f t.def in
    {t with use; def}

  let replace ~target b t = map (fun a -> if Box.equal a target then b else a) t

  let pp pp_box ppf t =
    let get xs i = try Array.get xs i with _ -> raise (Invalid_index i) in
    Fmt.pf ppf "%a" (t.formatter ~use:(get t.use) ~def:(get t.def) pp_box) ()

  let case ~move ~otherwise t =
    if t.is_move then move t.def.(0) t.use.(0) else otherwise t

  module Key = struct
    type nonrec t = t

    let compare = compare
  end

  module Set = Set.Make (Key)
  module Map = Map.Make (Key)
end

type asm = Asm.t

let%test_module _ =
  ( module struct
    open Asm

    (* Some registers. *)
    let a = box ()
    and b = box ()
    and c = box ()

    let pp_reg ppf r =
      if Box.equal r a then Fmt.text ppf "a"
      else if Box.equal r b then Fmt.text ppf "b"
      else if Box.equal r c then Fmt.text ppf "c"
      else Box.pp ppf r

    let t1 =
      v ~is_move:true ~use:[b] ~def:[a] (fun ~use ~def pp_box ppf () ->
          Fmt.pf ppf "@[<h>mov@ %a,@ %a@]" pp_box (def 0) pp_box (use 0) )

    let t2 =
      v ~use:[a; b] ~def:[b] (fun ~use ~def pp_box ppf () ->
          Fmt.pf ppf "@[<h>add@ %a,@ %a@]" pp_box (def 0) pp_box (use 0) )

    let test pp_box t = Fmt.pr "%a@." (pp pp_box) t

    let%test_module "pp" =
      ( module struct
        let%expect_test _ = test Box.pp t1 ; [%expect {| mov #0, #1 |}]
        let%expect_test _ = test pp_reg t1 ; [%expect {| mov a, b |}]
        let%expect_test _ = test pp_reg t2 ; [%expect {| add b, a |}]
      end )

    let%expect_test "modify_use" =
      test pp_reg (modify_use (fun use -> use.(0) <- c) t1) ;
      [%expect {| mov a, c |}]

    let%expect_test "modify_def" =
      test pp_reg (modify_def (fun def -> def.(0) <- c) t2) ;
      [%expect {| add c, a |}]

    let%expect_test "map" =
      test pp_reg (map (fun _ -> a) t1) ;
      [%expect {| mov a, a |}]

    let%expect_test "replace" =
      test pp_reg (replace ~target:b c t2) ;
      [%expect {| add c, a |}]

    let%test_module "case" =
      ( module struct
        let%test _ =
          case t1 ~move:(fun _dst _src -> true) ~otherwise:(fun _ -> false)

        let%test _ = case t2 ~move:(fun _ _ -> false) ~otherwise:(fun _ -> true)
      end )

    let%test_module "compare" =
      ( module struct
        let%test _ = compare t1 t1 = 0 let%test _ = compare t1 t2 <> 0
      end )

    let show xs = Fmt.pr "%a@." (Fmt.hbox (Iter.pp pp_reg)) xs

    let%test_module "use" =
      ( module struct
        let%expect_test _ =
          show (use t1) ;
          [%expect {| b |}]

        let%expect_test _ =
          show (use t2) ;
          [%expect {| ab |}]
      end )

    let%test_module "def" =
      ( module struct
        let%expect_test _ =
          show (def t1) ;
          [%expect {| a |}]

        let%expect_test _ =
          show (def t2) ;
          [%expect {| b |}]
      end )

    let%test "[An invalid index raises an exception]" =
      let t =
        Asm.v ~use:[a; b] ~def:[a] (fun ~use ~def pp_box ppf () ->
            Fmt.pf ppf "@[<h>foo@ %a,@ %a@]" pp_box (def 0) pp_box (use 8) )
      in
      try test pp_reg t ; false with Invalid_index 8 -> true
  end )
