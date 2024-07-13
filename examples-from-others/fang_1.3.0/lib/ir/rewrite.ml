(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Prelude

module type REWRITE = sig
  type _ t
  type _ observation

  val observe : 'a t -> 'a observation

  include IR with type 'a t := 'a t
end

module type TRANSFORM = sig
  type _ from
  type _ term

  val forward : 'a from -> 'a term
  val backward : 'a term -> 'a from
end

module Identity (X : TRANSFORM) (Next : REWRITE with type 'a t = 'a X.from) =
struct
  open X

  type 'a t = 'a term
  type 'a observation = 'a Next.observation

  let const x = forward (Next.const x)
  let loc l = forward (Next.loc l)
  let box b = forward (Next.box b)
  let mem v = forward (Next.mem (backward v))
  let perform e v = forward (Next.perform (backward e) (backward v))
  let call v vs = forward (Next.call (backward v) (List.map backward vs))
  let add v1 v2 = forward (Next.add (backward v1) (backward v2))
  let sub v1 v2 = forward (Next.sub (backward v1) (backward v2))
  let mul v1 v2 = forward (Next.mul (backward v1) (backward v2))
  let div v1 v2 = forward (Next.div (backward v1) (backward v2))
  let move v1 v2 = forward (Next.move (backward v1) (backward v2))
  let def l = forward (Next.def l)
  let discard v = forward (Next.discard (backward v))
  let seq e1 e2 = forward (Next.seq (backward e1) (backward e2))

  let cjump r v1 v2 pos neg =
    forward (Next.cjump r (backward v1) (backward v2) pos neg)

  let jump l = forward (Next.jump l)
  let observe x = Next.observe (backward x)
end

module Lift (I : IR) = struct
  include I

  type 'a observation = 'a I.t

  let observe x = x
end
