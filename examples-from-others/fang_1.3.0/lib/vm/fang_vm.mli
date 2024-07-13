(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

(** Abstractions for a virtual machine.

    The intermediate representation for programs assumes a generic ("virtual")
    machine with these features:

    - efficient temporary storage locations
    - a way to refer to particular memory locations.

    Later, when targeting a particular hardware architecture, these are replaced
    with architecture-specific registers and assembler labels. *)

(** {1 Temporary storage locations}*)

(** A handle to a temporary storage location (a "box").

    These storage locations are like registers, except there are no limits on
    how many may be allocated (unlike actual hardware architectures in which
    there are a finite number of registers). *)
type box

val box : unit -> box
(** Allocate a new temporary storage location.

    A newly-allocated box is distinct from all previously-allocated boxes.
    Allocating a box is cheap. *)

(** Operations on temporary storage locations. *)
module Box : sig
  type t = box

  val equal : t -> t -> bool
  (** [equal b1 b2] is [true] if [b1] and [b2] refer to the same box. *)

  val compare : t -> t -> int
  (** If [equal b1 b2] is [true] then [compare b1 b2] is [0]. Otherwise, there
      exists a total order on boxes but it is unspecified. *)

  val pp : t Fmt.t
    [@@ocaml.toplevel_printer]
  (** Format a box.

      The format is unspecified, but if [equal b1 b2] is [true] then [b1] and
      [b2] will have the same textual representation. Otherwise, they will be
      distinguishable by humans. *)

  module Set : Set.S with type elt = box
  module Map : Map.S with type key = box
end

(** {1 Labels for memory locations} *)

(** A label for a location in program memory.

    There are two types of labels:

    - "global" labels denote named parts of the program (such as subroutiones)
      where the name itself is meaningful.
    - "local" labels are anonymous. They have distinct names, but these names
      are generated in an unspecified way. Local labels are useful for denoting
      parts of a program for later reference but where the name itself is
      irrelevant. *)
type label

val local_label : unit -> label
(** Generate a new local label that is guaranteed to be distinct from all
    previously-generated local labels. *)

val global_label : string -> label
(** [global_label name] is the global label with the name [name]. *)

(** Operations on labels. *)
module Label : sig
  type t = label

  val equal : t -> t -> bool
  (** Labels are compared for equality based on whether they're local or global:

      - If [l1] and [l2] are both global, then [equal l1 l2] is [true] when the
        strings comprising their names are equal.

      - If [l1] and [l2] are both local, then [equal l1 l2] is [true] when [l1]
        and [l2] are handles to the same local label.

      - Otherwise, [equal _ _] is [false]. *)

  val compare : t -> t -> int
  (** If [equal l1 l2] is [true] then [compare l1 l2] is [0]. Otherwise, there
      exists a total order on labels but it is unspecified. *)

  val pp : ?local:int Fmt.t -> ?global:string Fmt.t -> unit -> t Fmt.t
  (** Format a label.

      By default, labels are formatted as by {!pp_top}. The way that local and
      global labels are formatted can be overridden. *)

  val pp_top : t Fmt.t
    [@@ocaml.toplevel_printer]
  (** Format a label in a default human-friendly way.

      This format may not correspond to anything that an actual assembler
      recognizes.

      A local label is formatted as [.L%d] where [%d] is a unique integer.

      A global label is formatted as its name. *)
end
