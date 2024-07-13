(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

(** Manipulate and pretty-print assembly instructions.

    Assembly instructions are defined by how they are pretty-printed but can be
    altered after construction to "swap out" different registers. This is
    useful, for example, for assigning real processor registers to the temporary
    storage locations in assembly emitted by earlier stages of the compiler
    ("coloring").

    Instructions are annotated in a limited way so that they can analyzed by
    different stages of the compiler. For example, registers are marked as being
    read ("use") and/or written to ("def") by the instruction.

    An {{!section:example} example} might be helpful. *)

open Fang_iter
open Fang_vm

(** A single assembly instruction.

    All instructions are distinct, even if they are pretty-printed identically
    to other instructions. *)
type asm

(** Assembly instructions. *)
module Asm : sig
  type t = asm

  exception Invalid_index of int

  (** {1 Defining instructions} *)

  (** Selects a register by its zero-based index in "use" and "def" lists. These
      are not defined by callers: they're provided automatically to help define
      the instruction pretty-printer.

      Raises {!Invalid_index} if the index is out-of-bounds of the use- or
      def-list. *)
  type selector = int -> box

  (** A function which defines a pretty-printer for an instruction given:

      - An indexed sequence of "use" registers
      - An indexed sequence of "def" registers
      - A pretty-printer for registers *)
  type formatter = use:selector -> def:selector -> box Fmt.t -> unit Fmt.t

  val v : ?is_move:bool -> ?use:box list -> ?def:box list -> formatter -> t
  (** Define a new instruction.

      If [is_move] is [true] (its default value is [false]), then this indicates
      that the only behaviour of this instruction is to copy a value from one
      register to another. *)

  (** {1 Operations on instructions} *)

  val compare : t -> t -> int
  (** [compare t1 t2] is [0] if [t1] and [t2] refer to the same value produced
      by {!v}. Otherwise, there exists a total order on instructions but it is
      unspecified. *)

  val pp : box Fmt.t -> t Fmt.t

  (** {1 Transforming instructions} *)

  val modify_use : (box array -> unit) -> t -> t
  val modify_def : (box array -> unit) -> t -> t
  val map : (box -> box) -> t -> t
  val replace : target:box -> box -> t -> t

  (** {1 Querying instructions} *)

  val use : t -> box iter
  val def : t -> box iter
  val case : move:(box -> box -> 'a) -> otherwise:(t -> 'a) -> t -> 'a

  (** {1 Containers of instructions} *)

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end

(** {1:example Example}

    Suppose we wish to represent the instruction

    {v add x, y v}

    (which adds the value in the register [x] to the value in the register [y]
    and stores the result back in [x]) for some registers [x] and [y] defined as
    follows:

    {[
      let x = box ()
      and y = box ()
    ]}

    The instruction can be defined as

    {[
      let t =
        Asm.v ~use:[x; y] ~def:[x] (fun ~use ~def pp_box ppf () ->
            Fmt.pf ppf "@[<h>add@ %a,@ %a@]" pp_box (use 0) pp_box (use 1) )
    ]}

    [x] and [y] have been marked as "use" because the instruction reads their
    values. Only [x] has been marked as "def" ("defined") because only [x] is
    written to ([y] is unchanged by the instruction).

    The instruction is defined by specifying a pretty-printer for it. Instead of
    referring to the registers [x] and [y] directly we use the [pp_box]
    formatter that is provided and refer to the registers by their index in the
    [use] and [def] lists.

    The reason for this complexity is to make it easier to "swap out" different
    registers without having to reconstruct the instruction.

    For example, here's a pretty-printer for our registers [x] and [y]

    {[
      let pp_reg ppf b =
        if Box.equal b x then Fmt.text ppf "x"
        else if Box.equal b y then Fmt.text ppf "y"
        else Box.pp ppf b
    ]}

    with which we can pretty-print the instruction:

    {[ Fmt.strf "%a" (Asm.pp pp_reg) t ]}

    The result is ["add x, y"].

    Now, let's replace [y] with [x]:

    {[ let t2 = Asm.replace ~target:y x t ]}

    The result of

    {[ Fmt.strf "%a" (Asm.pp pp_reg) t2 ]}

    is ["add x, x"]. *)
