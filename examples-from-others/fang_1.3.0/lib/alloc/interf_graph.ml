(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_iter
open Fang_vm
include Undirected_graph.Make (Box)

let pp_dot pp_reg ppf t =
  let pp_vertex ppf v = Fmt.pf ppf "@[<h>%a;@]" (Fmt.quote pp_reg) v in
  let pp_edge ppf (v1, v2) =
    Fmt.pf ppf "@[<h>%a--%a;@]" (Fmt.quote pp_reg) v1 (Fmt.quote pp_reg) v2
  in
  Fmt.(
    const string "graph"
    ++ braces
         (vbox
            ( const (Iter.pp pp_vertex) (vertices t)
            ++ sp
            ++ const (Iter.pp pp_edge) (edges t) ) ))
    ppf ()
