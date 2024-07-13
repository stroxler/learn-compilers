(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_vm

type value = private Value_tag
type effect = private Effect_tag

type rel =
  [`Equal | `Not_equal | `Less | `Less_or_equal | `Greater | `Greater_or_equal]

module type IR = sig
  type _ t

  val const : int64 -> value t
  val add : value t -> value t -> value t
  val sub : value t -> value t -> value t
  val mul : value t -> value t -> value t
  val div : value t -> value t -> value t
  val box : box -> value t
  val mem : value t -> value t
  val loc : label -> value t
  val perform : effect t -> value t -> value t
  val call : value t -> value t list -> value t
  val move : value t -> value t -> effect t
  val def : label -> effect t
  val discard : value t -> effect t
  val seq : effect t -> effect t -> effect t
  val cjump : rel -> value t -> value t -> label -> label -> effect t
  val jump : label -> effect t
end
