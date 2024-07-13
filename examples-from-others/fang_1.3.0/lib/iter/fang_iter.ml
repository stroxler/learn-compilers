(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

type 'a iter = ('a -> unit) -> unit

module Iter = struct
  type 'a t = 'a iter

  let empty _ = ()
  let one x k = k x

  let pp ?(sep = Fmt.cut) pp_item ppf t =
    let first = ref true in
    t (fun x ->
        if not !first then sep ppf () else first := false ;
        pp_item ppf x )

  let adapt f z k = f k z
  let adapt2 f z k = f (fun a b -> k (a, b)) z
  let map f t k = t (fun x -> k (f x))
  let filter p t k = t (fun x -> if p x then k x)

  let filter_map f t k =
    t (fun x -> match f x with Some y -> k y | None -> ())

  let flat_map f t k = t (fun x -> f x k)
  let concat t1 t2 k = t1 k ; t2 k
  let cons x t k = k x ; t k

  let find_first (type a) p (t : a t) =
    let exception Early of a in
    try
      t (fun x -> if p x then raise_notrace (Early x)) ;
      None
    with Early x -> Some x

  let fold y f t =
    let y = ref y in
    t (fun x -> y := f !y x) ;
    !y

  let into_set (type a b) (module K : Set.S with type elt = a and type t = b) t
      =
    fold K.empty (Fun.flip K.add) t

  let into_list t = List.rev (fold [] (fun xs x -> x :: xs) t)
  let count t = fold 0 (fun n _ -> n + 1) t

  let first t =
    let exception Early in
    let r = ref None in
    try
      t (fun x ->
          r := Some x ;
          raise_notrace Early ) ;
      !r
    with Early -> !r

  let all p t =
    let exception Early in
    try
      t (fun x -> if not (p x) then raise_notrace Early) ;
      true
    with Early -> false

  let maximum compare t =
    let r = ref None in
    t (fun x ->
        match !r with
        | None -> r := Some x
        | Some y -> if compare x y > 0 then r := Some x ) ;
    !r
end

let%test_module _ =
  ( module struct
    open Iter

    let items xs = adapt List.iter xs
    let show t = Fmt.pr "%a" (Fmt.brackets (pp ~sep:Fmt.comma Fmt.int)) t

    let%expect_test "empty" = show empty ; [%expect {| [] |}]

    let%expect_test "one" =
      show (one 10) ;
      [%expect {| [10] |}]

    let%expect_test "adapt" =
      show (adapt List.iter [1; 2; 3; 4]) ;
      [%expect {| [1, 2, 3, 4] |}]

    let%expect_test "adapt2" =
      let show2 t =
        Fmt.pr "%a"
          (Fmt.brackets (pp ~sep:Fmt.comma Fmt.(Dump.pair int string)))
          t in
      let module M = Map.Make (Int) in
      let pairs = M.(empty |> add 1 "a" |> add 2 "b" |> add 3 "c") in
      show2 (adapt2 M.iter pairs) ;
      [%expect {| [(1, a), (2, b), (3, c)] |}]

    let%expect_test "map" =
      show (map (fun x -> x * 2) (items [1; 2; 3; 4])) ;
      [%expect {| [2, 4, 6, 8] |}]

    let%expect_test "filter" =
      show (filter (fun x -> x mod 2 = 0) (items [1; 2; 3; 4; 5])) ;
      [%expect {| [2, 4] |}]

    let%expect_test "filter_map" =
      show
        (filter_map
           (fun x -> if x mod 2 = 0 then Some (2 * x) else None)
           (items [1; 2; 3; 4; 5]) ) ;
      [%expect {| [4, 8] |}]

    let%expect_test "flat_map" =
      let three x = items [x; x; x] in
      show (flat_map three (items [1; 2; 3])) ;
      [%expect {| [1, 1, 1, 2, 2, 2, 3, 3, 3] |}]

    let%expect_test "concat" =
      show (concat (items [1; 2; 3]) (items [4; 5; 6])) ;
      [%expect {| [1, 2, 3, 4, 5, 6] |}]

    let%expect_test "cons" =
      show (cons 1 (cons 2 (cons 3 empty))) ;
      [%expect {| [1, 2, 3] |}]

    let%test_module "find_first" =
      ( module struct
        let xs = items [1; 3; 5; 7]

        let%test _ = find_first (fun x -> x mod 2 = 0) xs = None
        let%test _ = find_first (fun x -> x > 2) xs = Some 3
      end )

    let%test "fold" = fold 0 (fun sum x -> sum + x) (items [1; 2; 3; 4]) = 10

    let%test "into_set" =
      let module S = Set.Make (Int) in
      S.equal
        (into_set (module S) (items [1; 2; 1; 3; 2]))
        (S.of_list [1; 2; 3])

    let%test "into_list" = into_list (items [1; 2; 3]) = [1; 2; 3]
    let%test "count" = count (items [1; 2; 3]) = 3

    let%test_module "first" =
      ( module struct
        let%test _ = first empty = None let%test _ = first (one 10) = Some 10
      end )

    let%test_module "all" =
      ( module struct
        let xs = items [1; 2; 3; 4; 5]

        let%test _ = all (fun x -> x < 3) xs = false
        let%test _ = all (fun x -> x < 10) xs = true
      end )

    let%test_module "maximum" =
      ( module struct
        let%test _ = maximum Int.compare empty = None
        let%test _ = maximum Int.compare (items [1; 3; 2]) = Some 3
      end )
  end )
