(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_iter

module type VERTEX = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type t
  type vertex
  type edge = vertex * vertex

  val empty : t
  val add_vertex : vertex -> t -> t
  val add_edge : vertex -> vertex -> t -> t
  val vertices : t -> vertex iter
  val edges : t -> edge iter

  exception Missing_vertex of vertex

  val degree : vertex -> t -> int
  val adjacent : vertex -> t -> vertex iter
  val has_edge : vertex -> vertex -> t -> bool
end

(* The graph is stored as an adjacency-list (i.e., a mapping from each vertex to
   a list of its adjacent vertices) and also a set of edges.

   The separate edge list is stored because it makes {! S.edges} trivial to
   implement.

   However, since the graph is undirected how do we distinguish an edge [(v1,
   v2)] and [(v2, v1)]? We don't. Instead, we "normalize" edges so that the
   "left" vertex is always less than or equal to the "right" one. *)
module Make (V : VERTEX) = struct
  type vertex = V.t
  type edge = vertex * vertex

  let normalize_edge ((v1, v2) as e) =
    let c = V.compare v1 v2 in
    if c <= 0 then e else (v2, v1)

  let compare_edge (u1, u2) (v1, v2) =
    let c1 = V.compare u1 v1 in
    if c1 <> 0 then c1 else V.compare u2 v2

  module Vertex_map = Map.Make (V)

  module Edge_set = Set.Make (struct
    type t = edge

    let compare = compare_edge
  end)

  module Vertex_set = Set.Make (V)

  type t = {vertices: Vertex_set.t Vertex_map.t; edges: Edge_set.t}

  let empty = {vertices= Vertex_map.empty; edges= Edge_set.empty}

  let add_vertex v t =
    let vertices =
      t.vertices
      |> Vertex_map.update v (function
           | None -> Some Vertex_set.empty
           | Some _ as r -> r ) in
    {t with vertices}

  let add_edge v1 v2 t =
    let vertices =
      t.vertices
      |> Vertex_map.update v1 (function
           | None -> Some (Vertex_set.singleton v2)
           | Some vs -> Some (Vertex_set.add v2 vs) )
      |> Vertex_map.update v2 (function
           | None -> Some (Vertex_set.singleton v1)
           | Some vs -> Some (Vertex_set.add v1 vs) )
    and edges = Edge_set.add (normalize_edge (v1, v2)) t.edges in
    {vertices; edges}

  let vertices =
    let iter_vertices f t = Vertex_map.iter (fun v _ -> f v) t.vertices in
    fun t -> Iter.adapt iter_vertices t

  exception Missing_vertex of vertex

  let find_vertex v t =
    try Vertex_map.find v t.vertices with Not_found -> raise (Missing_vertex v)

  let edges t = Iter.adapt Edge_set.iter t.edges
  let degree v t = Vertex_set.cardinal (find_vertex v t)
  let adjacent v t = Iter.adapt Vertex_set.iter (find_vertex v t)
  let has_edge u v t = Edge_set.mem (normalize_edge (u, v)) t.edges
end

let%test_module _ =
  ( module struct
    open Make (Int)

    (* For convenience. *)
    let edges t = Iter.into_list (edges t)
    let vertices t = Iter.into_list (vertices t)
    let describe t = (vertices t, edges t)

    let%test "empty" = describe empty = ([], [])

    let%test_module "add_vertex" =
      ( module struct
        let%test _ = describe (empty |> add_vertex 5) = ([5], [])

        let%test _ =
          describe (empty |> add_vertex 10 |> add_vertex 5) = ([5; 10], [])

        let%test "[Adding an already-present vertex doesn't change the graph]" =
          describe (empty |> add_edge 5 10 |> add_edge 10 20 |> add_vertex 10)
          = ([5; 10; 20], [(5, 10); (10, 20)])
      end )

    let%test_module "add_edge" =
      ( module struct
        let%test _ = describe (empty |> add_edge 1 2) = ([1; 2], [(1, 2)])

        let%test _ =
          describe (empty |> add_vertex 1 |> add_vertex 2 |> add_edge 1 2)
          = ([1; 2], [(1, 2)])

        let%test "[Adding an already-present edge doesn't change the graph]" =
          describe
            ( empty |> add_edge 1 2 |> add_edge 2 3 |> add_edge 3 4
            |> add_edge 2 3 )
          = ([1; 2; 3; 4], [(1, 2); (2, 3); (3, 4)])
      end )

    let%test_module "degree" =
      ( module struct
        let%test _ = degree 5 (empty |> add_vertex 5) = 0
        let%test _ = degree 1 (empty |> add_edge 1 2) = 1
        let%test _ = degree 1 (empty |> add_edge 1 2 |> add_edge 1 3) = 2

        let%test _ =
          try
            ignore (degree 1 empty) ;
            false
          with Missing_vertex 1 -> true
      end )

    let%test_module "adjacent" =
      ( module struct
        let adjacent v t = Iter.into_list (adjacent v t)

        let%test _ = adjacent 1 (empty |> add_vertex 1) = []
        let%test _ = adjacent 1 (empty |> add_edge 1 2) = [2]
        let%test _ = adjacent 1 (empty |> add_edge 1 2 |> add_edge 1 3) = [2; 3]

        let%test _ =
          try
            ignore (adjacent 3 (empty |> add_edge 1 2)) ;
            false
          with Missing_vertex 3 -> true
      end )

    let%test_module "has_edge" =
      ( module struct
        let%test _ = has_edge 1 2 empty = false

        let%test _ =
          has_edge 1 2 (empty |> add_vertex 1 |> add_vertex 2) = false

        let%test _ = has_edge 1 2 (empty |> add_edge 2 1) = true
        let%test _ = has_edge 1 2 (empty |> add_edge 1 2 |> add_edge 3 4) = true
      end )
  end )
