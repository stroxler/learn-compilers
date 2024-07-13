(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

type box = int

let next_box_index = ref 0

let box () =
  let index = !next_box_index in
  incr next_box_index ; index

module Box = struct
  type t = box

  let equal = Int.equal
  let compare = Int.compare
  let pp ppf t = Fmt.(box (styled `Bold (const string "#" ++ int))) ppf t

  module Set = Set.Make (Int)
  module Map = Map.Make (Int)
end

let%test_module "box" =
  ( module struct
    open Box

    let%test_module "equal" =
      ( module struct
        let%test _ = not (equal (box ()) (box ()))

        let%test _ =
          let b = box () in
          equal b b
      end )

    let%test_module "compare" =
      ( module struct
        let b1 = box ()
        and b2 = box ()

        let%test _ = compare b1 b1 = 0
        let%test _ = compare b1 b2 <> 0
        let%test _ = compare b1 b2 <> compare b2 b1
      end )

    let%expect_test "pp" =
      Fmt.pr "%a" pp (box ()) ;
      [%expect {| #5 |}]
  end )

let next_label_index = ref 0

type label = Local of int | Global of string

let local_label () =
  let index = !next_label_index in
  incr next_label_index ; Local index

let global_label name = Global name

module Label = struct
  type t = label

  let equal t1 t2 =
    match (t1, t2) with
    | Local index1, Local index2 -> Int.equal index1 index2
    | Global name1, Global name2 -> String.equal name1 name2
    | _ -> false

  let compare t1 t2 =
    match (t1, t2) with
    | Local index1, Local index2 -> Int.compare index1 index2
    | Local _, Global _ -> -1
    | Global name1, Global name2 -> String.compare name1 name2
    | Global _, Local _ -> 1

  let pp ?local ?global () ppf t =
    match t with
    | Local index -> (
      match local with
      | None ->
          Fmt.(box (styled (`Fg `Blue) (const string ".L" ++ int))) ppf index
      | Some pp_local -> Fmt.(box pp_local) ppf index )
    | Global name -> (
      match global with
      | None -> Fmt.(box (styled (`Fg `Blue) string)) ppf name
      | Some pp_global -> Fmt.(box pp_global) ppf name )

  let pp_top = pp ()
end

let%test_module "label" =
  ( module struct
    open Label

    let%test_module "equal" =
      ( module struct
        let%test _ = not (equal (local_label ()) (local_label ()))

        let%test _ =
          let l = local_label () in
          equal l l

        let%test _ = not (equal (local_label ()) (global_label "foo"))
        let%test _ = equal (global_label "foo") (global_label "foo")
        let%test _ = not (equal (global_label "foo") (global_label "bar"))
      end )

    let%test_module "compare" =
      ( module struct
        let l1 = local_label ()
        and l2 = local_label ()
        and l3 = global_label "foo"
        and l4 = global_label "bar"

        let%test _ = compare l1 l1 = 0
        let%test _ = compare l3 l3 = 0
        let%test _ = compare l1 l2 <> 0
        let%test _ = compare l1 l3 <> 0
        let%test _ = compare l3 l4 <> 0
      end )

    let%test_module "pp_top" =
      ( module struct
        let%expect_test _ =
          Fmt.pr "%a" pp_top (local_label ()) ;
          [%expect {| .L6 |}]

        let%expect_test _ =
          Fmt.pr "%a" pp_top (global_label "foo") ;
          [%expect {| foo |}]
      end )

    let%test_module "pp" =
      ( module struct
        let%expect_test _ =
          Fmt.pr "%a" (pp ~local:Fmt.int ()) (local_label ()) ;
          [%expect {| 7 |}]

        let%expect_test _ =
          Fmt.pr "%a" (pp ~global:Fmt.(quote string) ()) (global_label "foo") ;
          [%expect {| "foo" |}]
      end )
  end )
