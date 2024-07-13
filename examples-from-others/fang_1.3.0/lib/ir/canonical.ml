(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_vm
open Prelude
open Rewrite

module Fold_constants (Next : REWRITE) = struct
  module Pass (I : IR) = struct
    module X = struct
      type 'a from = 'a I.t
      type _ term = Unk : 'a from -> 'a term | Const : int64 -> value term

      let forward z = Unk z

      let backward : type a. a term -> a from = function
        | Unk z -> z
        | Const c -> I.const c
    end

    module Delta = struct
      open X

      let const c = Const c

      let rec add v1 v2 =
        match (v1, v2) with
        | Const 0L, _ -> v2
        | _, Const 0L -> v1
        | _, Const c when c < 0L -> sub v1 (const (Int64.neg c))
        | Const c1, Const c2 -> const (Int64.add c1 c2)
        (* Constants on the right. *)
        | Const c, _ -> add v2 (const c)
        | _ -> forward I.(add (backward v1) (backward v2))

      and sub v1 v2 =
        match (v1, v2) with
        | _, Const c when c < 0L -> add v1 (const (Int64.neg c))
        | _, Const 0L -> v1
        | Const c1, Const c2 -> const (Int64.sub c1 c2)
        | _ -> forward I.(sub (backward v1) (backward v2))

      let rec mul v1 v2 =
        match (v1, v2) with
        | Const 0L, _ | _, Const 0L -> const 0L
        | Const 1L, _ -> v2
        | _, Const 1L -> v1
        | Const c1, Const c2 -> const (Int64.mul c1 c2)
        (* Constants on the left. *)
        | _, Const c -> mul (const c) v1
        | _ -> forward I.(mul (backward v1) (backward v2))

      let div v1 v2 =
        match (v1, v2) with
        | Const 0L, Const x when x <> 0L -> const 0L
        | _, Const 1L -> v1
        | Const c1, Const c2 when c2 <> 0L -> const (Int64.div c1 c2)
        | _ -> forward I.(div (backward v1) (backward v2))

      let cjump r v1 v2 pos neg =
        match (v1, v2) with
        | Const c1, Const c2 ->
            let f =
              match r with
              | `Equal -> ( = )
              | `Not_equal -> ( <> )
              | `Less -> ( < )
              | `Less_or_equal -> ( <= )
              | `Greater -> ( > )
              | `Greater_or_equal -> ( >= ) in
            let where = if f (Int64.compare c1 c2) 0 then pos else neg in
            forward I.(jump where)
        | _ -> forward I.(cjump r (backward v1) (backward v2) pos neg)
    end
  end

  module P = Pass (Next)
  include Identity (P.X) (Next)
  include P.Delta
end

(** The result of calling a subroutine is always stored in the same location
    (likely a register). Therefore, in a value like
    [add (call (...)) (call (...))], the second [call] will overwrite the return
    value of the first.

    The solution is to always store the result of [call] in a register. *)
module Store_call_result (Next : REWRITE) = struct
  module Pass (I : IR) = struct
    module X = struct
      type 'a from = 'a I.t
      type 'a term = 'a from

      let forward = Fun.id
      let backward = Fun.id
    end

    module Delta = struct
      open X

      let call v vs =
        let b = box () in
        I.(
          perform
            (move (box b) (call (backward v) (List.map backward vs)))
            (box b))
    end
  end

  module P = Pass (Next)
  include Identity (P.X) (Next)
  include P.Delta
end

module Eliminate_constant_discard (Next : REWRITE) = struct
  module Pass (I : IR) = struct
    module X = struct
      type 'a from = 'a I.t

      type _ term =
        | Const : int64 -> value term
        | Discard_const : int64 -> effect term
        | Unk : 'a from -> 'a term

      let forward z = Unk z

      let backward : type a. a term -> a from = function
        | Const c -> I.const c
        | Discard_const c -> I.(discard (const c))
        | Unk z -> z
    end

    module Delta = struct
      open X

      let const c = Const c

      let discard v =
        match v with
        | Const c -> Discard_const c
        | _ -> forward I.(discard (backward v))

      let perform e v =
        match e with
        | Discard_const _ -> v
        | _ -> forward I.(perform (backward e) (backward v))

      let seq e1 e2 =
        match (e1, e2) with
        | Discard_const _, _ -> e2
        | _, Discard_const _ -> e1
        | _ -> forward I.(seq (backward e1) (backward e2))
    end
  end

  module P = Pass (Next)
  include Identity (P.X) (Next)
  include P.Delta
end

module Lift_perform (Next : REWRITE) = struct
  module Pass (I : IR) = struct
    module X = struct
      type 'a from = 'a I.t

      type _ term =
        | Perform : effect from * value from -> value term
        | Unk : 'a from -> 'a term

      let forward z = Unk z

      let backward : type a. a term -> a from = function
        | Perform (e, v) -> I.perform e v
        | Unk z -> z
    end

    module Delta = struct
      open X

      let perform e v =
        match v with
        | Perform (x, y) -> Perform (I.(seq (backward e) x), y)
        | Unk _ -> Perform (backward e, backward v)

      let mem v =
        match v with
        | Perform (x, y) -> perform (forward x) (forward I.(mem y))
        | Unk _ -> forward I.(mem (backward v))

      let discard v =
        match v with
        | Perform (x, y) -> forward I.(seq x (discard y))
        | Unk _ -> forward (I.discard (backward v))

      let move v1 v2 =
        match (v1, v2) with
        | Perform (x1, y1), Perform (x2, y2) ->
            forward I.(seq x1 (seq x2 (move y1 y2)))
        | Perform (x, y), _ -> forward I.(seq x (move y (backward v2)))
        | _, Perform (x, y) -> forward I.(seq x (move (backward v1) y))
        | _ -> forward I.(move (backward v1) (backward v2))

      let call v vs =
        let xs, ys =
          let rec loop xs ys = function
            | [] -> (List.rev xs, List.rev ys)
            | Perform (x, y) :: vs -> loop (x :: xs) (y :: ys) vs
            | v :: vs -> loop xs (backward v :: ys) vs in
          loop [] [] vs in
        let y, xs =
          match v with Perform (x, y) -> (y, x :: xs) | _ -> (backward v, xs)
        in
        match xs with
        | [] -> forward I.(call y ys)
        | [x] -> perform (forward x) (forward I.(call y ys))
        | x :: xs ->
            perform
              (forward (List.fold_left I.seq x xs))
              (forward I.(call y ys))

      let rec cjump rel v1 v2 pos neg =
        match (v1, v2) with
        | Perform (x, y), _ ->
            forward (I.seq x (backward (cjump rel (forward y) v2 pos neg)))
        | _, Perform (x, y) ->
            let b = box () in
            forward
              (I.seq
                 (backward (move (forward (I.box b)) v1))
                 (I.seq x
                    (backward
                       (cjump rel (forward (I.box b)) (forward y) pos neg) ) ) )
        | _ -> forward I.(cjump rel (backward v1) (backward v2) pos neg)

      let arith f v1 v2 =
        match (v1, v2) with
        | Perform (x, y), _ -> perform (forward x) (forward (f y (backward v2)))
        | _, Perform (x, y) ->
            let b = box () in
            perform
              (move (forward (I.box b)) v1)
              (perform (forward x) (forward (f (I.box b) y)))
        | _ -> forward (f (backward v1) (backward v2))

      let add v1 v2 = arith I.add v1 v2
      let sub v1 v2 = arith I.sub v1 v2
      let mul v1 v2 = arith I.mul v1 v2
      let div v1 v2 = arith I.div v1 v2
    end
  end

  module P = Pass (Next)
  include Identity (P.X) (Next)
  include P.Delta
end

(** This phase ensures that all nested [seq] are of the form:

    {v seq x1 (seq x2 (seq x3 seq (...))) v} *)
module Linearize (Next : REWRITE) = struct
  module Pass (I : IR) = struct
    module X = struct
      type 'a from = 'a I.t

      type _ term =
        | Unknown : 'a from -> 'a term
        | Seq : effect from * (effect from -> effect from) -> effect term

      let forward z = Unknown z

      let backward : type a. a term -> a from = function
        | Seq (x, f) -> f x
        | Unknown z -> z
    end

    module Delta = struct
      open X

      let last x = Seq (x, Fun.id)
      let move v1 v2 = last I.(move (backward v1) (backward v2))
      let def l = last I.(def l)
      let discard v = last I.(discard (backward v))

      let seq e1 e2 =
        match (e1, e2) with
        | Seq (x1, f1), Seq (x2, f2) ->
            Seq (x2, fun nil -> f1 I.(seq x1 (f2 nil)))
        | Seq (x, f), Unknown _ -> Seq (backward e2, fun nil -> f I.(seq x nil))
        | Unknown _, Seq (x, f) -> Seq (x, fun nil -> I.(seq x (f nil)))
        | Unknown _, Unknown _ -> forward I.(seq (backward e1) (backward e2))

      let cjump r v1 v2 pos neg =
        last I.(cjump r (backward v1) (backward v2) pos neg)

      let jump l = last I.(jump l)
    end
  end

  module P = Pass (Next)
  include Identity (P.X) (Next)
  include P.Delta
end

module Make (Next : REWRITE) = struct
  include
    Fold_constants
      (Store_call_result
         (Lift_perform (Eliminate_constant_discard (Linearize (Next)))))
end
