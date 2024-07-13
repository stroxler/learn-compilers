(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

module type STATE = sig
  type state
  type 'a t = state -> 'a * state

  val pure : 'a -> 'a t
  val get : state t
  val set : state -> unit t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val bind : ('a -> 'b t) -> 'a t -> 'b t
  val sequence : 'a t list -> 'a list t
  val traverse : ('a -> 'b t) -> 'a list -> 'b list t
  val traverse_unit : ('a -> unit t) -> 'a list -> unit t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  end
end

module State (K : sig
  type t
end) : STATE with type state = K.t = struct
  type state = K.t
  type 'a t = state -> 'a * state

  let pure x s = (x, s)
  let get s = (s, s)
  let set s _ = ((), s)

  let map f r s =
    let x, s = r s in
    (f x, s)

  let bind f r s =
    let x, s = r s in
    (f x) s

  module Syntax = struct
    let ( let* ) r f = bind f r
    let ( let+ ) r f = map f r

    let ( and+ ) r1 r2 s =
      let x1, s = r1 s in
      let x2, s = r2 s in
      ((x1, x2), s)

    let ( and* ) = ( and+ )
  end

  open Syntax

  let traverse f xs =
    let rec cps xs k =
      match xs with
      | [] -> k (pure [])
      | x :: xs ->
          let* y = f x in
          cps xs (fun ys ->
              let* ys = ys in
              k (pure (y :: ys)) ) in
    cps xs Fun.id

  let sequence xs = traverse Fun.id xs

  let traverse_unit f xs =
    let rec loop = function
      | [] -> pure ()
      | x :: xs ->
          let* () = f x in
          loop xs in
    loop xs
end

let%test_module "state" =
  ( module struct
    module M = State (struct type t = int end)
    open M
    open M.Syntax

    let gen =
      let* next = get in
      let+ () = set (next + 1) in
      next

    let%test "sequence" =
      let t = sequence [gen; gen; gen] in
      let xs, _ = t 0 in
      xs = [0; 1; 2]

    let%test "traverse" =
      let t =
        traverse
          (fun x ->
            let+ y = gen in
            x + y )
          [10; 20; 30] in
      let xs, _ = t 1 in
      xs = [11; 22; 33]

    let%test "traverse_unit" =
      let t =
        traverse_unit
          (fun x ->
            let* s = get in
            let* () = set (x * s) in
            let+ _ = gen in
            () )
          [2; 3; 4] in
      let (), s = t 5 in
      s = 137
  end )
