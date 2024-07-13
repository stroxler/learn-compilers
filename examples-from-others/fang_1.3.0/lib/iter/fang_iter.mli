(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

(** Efficient iteration over collections.

    These are so-called "internal" iterators. The caller provides a callback
    function which the collection invokes for each of its items.

    These iterators are less flexible than {!Seq.t} but should be more efficient
    since they allocate far less.

    This library is inspired by the {{:https://github.com/c-cube/iter} Iter}
    library by Simon Cruanes. In particular, there is a lot of useful discussion
    there on the trade-offs of this approach. *)

(** An iterator over a collection of values of type ['a]. *)
type 'a iter = ('a -> unit) -> unit

module Iter : sig
  type 'a t = 'a iter

  (** {1 Building iterators} *)

  val empty : _ t
  (** [empty] is an iterator which doesn't iterate over any values. *)

  val one : 'a -> 'a t
  (** [one x] iterates over the value [x] and no others. *)

  val adapt : (('a -> unit) -> 'b -> unit) -> 'b -> 'a t
  (** The modules for many standard containers already have a function for
      invoking a function to each item.

      For example,

      {[ List.iter (fun x -> print_int x) [1; 2; 3] ]}

      willy invoke [fun x -> print_int x] for each item in the list.

      [adapt f xs] produces an iterator for a container [xs] given an iteration
      function [f] like {!List.iter}.

      For example,

      {[ adapt List.iter [1; 2; 3] ]}

      is an [int t] which will iterate over the values [\[1; 2; 3\]]. *)

  val adapt2 : (('a -> 'b -> unit) -> 'c -> unit) -> 'c -> ('a * 'b) t

  val cons : 'a -> 'a t -> 'a t
  (** [cons x xs] iterates first over the value [x] and then values iterated
      over by [xs]. *)

  val concat : 'a t -> 'a t -> 'a t
  (** [concat xs ys] iterates over the values iterated over by [xs] and then the
      values iterated over by [ys]. *)

  (** {1 Transformations} *)

  val flat_map : ('a -> 'b t) -> 'a t -> 'b t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** {1 Searching} *)

  val find_first : ('a -> bool) -> 'a t -> 'a option
  val maximum : ('a -> 'a -> int) -> 'a t -> 'a option
  val all : ('a -> bool) -> 'a t -> bool
  val first : 'a t -> 'a option

  (** {1 Folding} *)

  val fold : 'b -> ('b -> 'a -> 'b) -> 'a t -> 'b
  val into_set : (module Set.S with type elt = 'a and type t = 'b) -> 'a t -> 'b
  val into_list : 'a t -> 'a list
  val count : _ t -> int

  (** {1 Pretty-printing} *)

  val pp : ?sep:unit Fmt.t -> 'a Fmt.t -> 'a t Fmt.t
end
