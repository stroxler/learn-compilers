(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

(** An intermediate representation (IR) for Tiger.

    As with Tiger, the IR is encoded in the tagless-final style. *)

open Fang_vm

(** {1 Language specification}

    The IR is a language that expresses programs as a "tree" of values and
    effects.

    For example, the following program prints the integers from [1] to [5] and
    produces the value [0] as a result.

    {v
(perform
 (seq
  (move (box #1) (const 1))
  (seq
   (def loop_start)
   (seq
    (cjump LE (box #1) (const 5) loop_body loop_end)
    (seq
     (def loop_body)
     (seq
      (discard (call (loc print_int) (box #1)))
      (seq
       (jump (loc loop_start) loop_start)
       (def loop_end)))))))
 (const 0))
    v} *)

type value = private Value_tag
type effect = private Effect_tag

type rel =
  [`Equal | `Not_equal | `Less | `Less_or_equal | `Greater | `Greater_or_equal]

module type IR = sig
  type _ t

  (** {1 Values}

      Values are 64 bit signed integers. They have type [value t]. *)

  val const : int64 -> value t
  (** A constant integer. *)

  val add : value t -> value t -> value t
  (** Addition. *)

  val sub : value t -> value t -> value t
  (** Subtraction. *)

  val mul : value t -> value t -> value t
  (** Multiplication. *)

  val div : value t -> value t -> value t
  (** Division. *)

  val box : box -> value t
  (** [box b] is the value currently stored in [b]. *)

  val mem : value t -> value t
  (** [mem v] is the value currently stored in memory at address [v]. *)

  val loc : label -> value t
  (** [loc l] is the address of the label [l] in memory. *)

  val perform : effect t -> value t -> value t
  (** [perform e v] is the value [v] after the effect [e] has been performed. *)

  val call : value t -> value t list -> value t
  (** [call v vs] is the result of invoking the subroutine at memory address [v]
      with the values [vs] as arguments. *)

  (** {1 Effects} *)

  val move : value t -> value t -> effect t
  (** Move a value into a location.

      [move v1 v2] is valid when [v1] evaluates to either:

      - [box b], which stores [v2] in [b]
      - [mem addr], which stores [v2] in memory at address [addr] *)

  val def : label -> effect t
  (** Define a label for the current memory address. *)

  val discard : value t -> effect t
  (** Ignore a value. *)

  val seq : effect t -> effect t -> effect t
  (** [seq e1 e2] is the effect of first performing [e1] and then performing
      [e2]. *)

  val cjump : rel -> value t -> value t -> label -> label -> effect t
  (** Continue execution at one of two labels, based on a relation between
      values.

      [cjump rel v1 v2 l1 l2] compares [v1] and [v2] according to [rel]. If the
      relation holds then execution continues at the location of [l1].
      Otherwise, execution continues at the location of [l2]. *)

  val jump : label -> effect t
  (** Continue execution at the location of a label.

      Note: Appel's [jump] is more complex: his takes the value of an address to
      jump to and a list of all possible labels that the address is referring
      to. The reason for this complexity is to support jumping to an address
      which is the result of a calculation, like a [switch] statement in the C
      programming language. For example, we may calculate the address to jump to
      from a base address and an offset. We must still know all the possible
      labels that the calculation could resolve to because that information is
      needed for data-flow analysis. Tiger doesn't need such functionality so we
      keep things simple. *)
end

(** {1 Interpreters} *)

(** Pretty-printing IR fragments. *)
module Pretty : sig
  (** A pretty-printer for an IR fragment. *)
  type _ t

  (** {1 Pretty-printing} *)

  val pp : box Fmt.t -> value t Fmt.t

  (** {1 Building pretty-printers} *)

  include IR with type 'a t := 'a t  (** @closed *)
end

(** In-memory storage of IR fragments. *)
module Tree : sig
  (** An in-memory tree representing the IR fragment. *)
  type _ t

  (** A read-only view of the in-memory representation. *)
  type view = private
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

  val view : _ t -> view

  (** {1 Building the in-memory tree} *)

  include IR with type 'a t := 'a t  (** @closed *)
end

(** {1 Rewriting engine}

    This is an implementation of the
    {{:http://okmij.org/ftp/tagless-final/course2/index.html} optimization
    framework} described by Oleg Kiselyov. *)

module type REWRITE = sig
  type _ t
  type _ observation

  val observe : 'a t -> 'a observation

  include IR with type 'a t := 'a t  (** @closed *)
end

module Lift (I : IR) : REWRITE with type 'a observation = 'a I.t

module type TRANSFORM = sig
  type _ from
  type _ term

  val forward : 'a from -> 'a term
  val backward : 'a term -> 'a from
end

module Identity (X : TRANSFORM) (Next : REWRITE with type 'a t = 'a X.from) :
  REWRITE with type 'a observation = 'a Next.observation

(** {1 Canonical IR} *)

module Canonical (Next : REWRITE) :
  REWRITE with type 'a observation = 'a Next.observation
