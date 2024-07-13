(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_monads
open Prelude
module Name_table = Map.Make (String)

module Make (L : TIGER) = struct
  module Table = Source_span.Map

  type state =
    { depth: int
    ; env: (int * Source_span.t) Name_table.t
    ; scopes: [`Local | `Escapes] Table.t }

  module R = State (struct type t = state end)
  open R
  open R.Syntax

  type 'a t = 'a L.t R.t

  let int x k = pure (L.int x k)
  let string x k = pure (L.string x k)
  let nil k = pure (L.nil k)

  let arith oper lhs rhs k =
    let+ lhs = lhs and+ rhs = rhs in
    L.arith oper lhs rhs k

  let neg expr k =
    let+ expr = expr in
    L.neg expr k

  let cmp rel lhs rhs k =
    let+ lhs = lhs and+ rhs = rhs in
    L.cmp rel lhs rhs k

  let value target k =
    let+ target = target in
    L.value target k

  let if_ ~condition ~yes ?no k s =
    let condition, s = condition s in
    let yes, s = yes s in
    let no, s =
      match no with
      | None -> (None, s)
      | Some no ->
          let no, s = no s in
          (Some no, s) in
    (L.if_ ~condition ~yes ?no k, s)

  let while_ ~condition ~body k =
    let+ condition = condition and+ body = body in
    L.while_ ~condition ~body k

  let for_ name ~initial ~final ~body k s =
    let initial, s = initial s in
    let final, s = final s in
    let env = Name_table.add name (s.depth, k) s.env
    and scopes = Table.add k `Local s.scopes in
    let s0 = s in
    let s = {s0 with env; scopes} in
    let body, s = body s in
    (L.for_ name ~initial ~final ~body k, {s0 with scopes= s.scopes})

  let break k = pure (L.break k)

  let seq exprs k =
    let+ exprs = sequence exprs in
    L.seq exprs k

  let call name args k =
    let+ args = sequence args in
    L.call name args k

  let uniform_array name ~size ~initial k =
    let+ size = size and+ initial = initial in
    L.uniform_array name ~size ~initial k

  let array name exprs k =
    let+ exprs = sequence exprs in
    L.array name exprs k

  let size expr k =
    let+ expr = expr in
    L.size expr k

  let record name bindings k =
    let+ bindings =
      traverse
        (fun (field_name, expr) ->
          let+ expr = expr in
          (field_name, expr) )
        bindings
    in
    L.record name bindings k

  let scope decls expr k s =
    let s0 = s in
    let decls, s = (sequence decls) s in
    let expr, s = expr s in
    (L.scope decls expr k, {s0 with scopes= s.scopes})

  let assign target expr k =
    let+ expr = expr and+ target = target in
    L.assign target expr k

  let name name k s =
    let defn_depth, defn_k = Name_table.find name s.env in
    let scopes =
      if defn_depth < s.depth then Table.add defn_k `Escapes s.scopes
      else s.scopes in
    (L.name name k, {s with scopes})

  let index target expr k =
    let+ target = target and+ expr = expr in
    L.index target expr k

  let access target name k =
    let+ target = target in
    L.access target name k

  let var name ?type_name expr k s =
    let expr, s = expr s in
    let env = Name_table.add name (s.depth, k) s.env
    and scopes = Table.add k `Local s.scopes in
    (L.var name ?type_name expr k, {s with env; scopes})

  let fns fs k =
    let+ fs = sequence fs in
    L.fns fs k

  let types ts k =
    let+ ts = sequence ts in
    L.types ts k

  let fn name params ?type_name body k s =
    let s0 = s in
    let s = {s0 with depth= s0.depth + 1} in
    let params, s =
      (traverse
         (fun (param, type_name) ->
           let+ param = param in
           (param, type_name) )
         params )
        s in
    let body, s = body s in
    (L.fn name params ?type_name body k, {s0 with scopes= s.scopes})

  let param name k s =
    let env = Name_table.add name (s.depth, k) s.env
    and scopes = Table.add k `Local s.scopes in
    (L.param name k, {s with env; scopes})

  let alias_type ~name ~target k = pure (L.alias_type ~name ~target k)
  let array_type ~name ~item k = pure (L.array_type ~name ~item k)
  let record_type name fields k = pure (L.record_type name fields k)

  let table t =
    let x, s = t {depth= 0; env= Name_table.empty; scopes= Table.empty} in
    (x, s.scopes)

  let analyze t =
    let x, scopes = table t in
    (scopes, x)
end
