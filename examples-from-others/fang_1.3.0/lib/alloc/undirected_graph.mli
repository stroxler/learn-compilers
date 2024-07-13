(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_iter

module type VERTEX = sig
  type t

  val compare : t -> t -> int
end

(** Generic undirected graphs with at most one edge between any two vertices. *)
module type S = sig
  type t
  type vertex
  type edge = vertex * vertex

  val empty : t

  val add_vertex : vertex -> t -> t
  (** [add_vertex v g] is the graph [g] with vertex [v] present.

      If [v] was not previously in the graph then it's added without any edges.
      Otherwise, the graph is unmodified. *)

  val add_edge : vertex -> vertex -> t -> t
  (** [add_edge vx vy g] is the graph [g] with vertices [vx] and [vy] present
      and connected by an edge.

      If either [vx] or [vy] were not previously in the graph then they're
      added. *)

  val vertices : t -> vertex iter
  val edges : t -> edge iter

  exception Missing_vertex of vertex

  val degree : vertex -> t -> int
  (** [degree v g] is the number of edges connected to vertex [v] in the graph
      [g].

      Raises {!Missing_vertex} if [v] is not present in [g]. *)

  val adjacent : vertex -> t -> vertex iter
  (** [adjacent v g] are the vertices connected to [v] by edges in the graph
      [g].

      Raises {!Missing_vertex} if [v] is not present in [g]. *)

  val has_edge : vertex -> vertex -> t -> bool
  (** [has_edge vx vy g] is [true] if [vx] and [vy] are connected by an edge in
      the graph [g].

      If either [vx] or [vy] are not present in the graph the result is [false]. *)
end

module Make (V : VERTEX) : S with type vertex = V.t
