(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_vm
include Prelude
module Pretty = Pretty

module Tree = struct
  type view =
    | Const of int64
    | Add of view * view
    | Sub of view * view
    | Mul of view * view
    | Div of view * view
    | Box of box
    | Mem of view
    | Loc of label
    | Perform of view * view
    | Call of view * view list
    | Move of view * view
    | Def of label
    | Discard of view
    | Seq of view * view
    | Cjump of rel * view * view * label * label
    | Jump of label

  type _ t = view

  let const c = Const c
  let add v1 v2 = Add (v1, v2)
  let sub v1 v2 = Sub (v1, v2)
  let mul v1 v2 = Mul (v1, v2)
  let div v1 v2 = Div (v1, v2)
  let box b = Box b
  let mem v = Mem v
  let loc l = Loc l
  let perform e v = Perform (e, v)
  let call v vs = Call (v, vs)
  let move v1 v2 = Move (v1, v2)
  let def l = Def l
  let discard v = Discard v
  let seq e1 e2 = Seq (e1, e2)
  let cjump rel v1 v2 pos neg = Cjump (rel, v1, v2, pos, neg)
  let jump l = Jump l
  let view = Fun.id
end

include Rewrite
module Canonical = Canonical.Make
