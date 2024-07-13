(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_monads
open Prelude

module Make (L : TIGER) = struct
  module Name_table = Map.Make (String)
  module Table = Source_span.Map

  type state =
    {env: Source_span.t Name_table.t; mutability: [`Constant | `Mutates] Table.t}

  module R = State (struct type t = state end)
  open R
  open R.Syntax

  type _ t = Name : string * target L.t -> target t | Unk : 'a L.t R.t -> 'a t

  let unwrap = function Name (_, x) -> pure x | Unk target -> target
  let int x k = Unk (pure (L.int x k))
  let string x k = Unk (pure (L.string x k))
  let nil k = Unk (pure (L.nil k))

  let arith oper (Unk lhs) (Unk rhs) k =
    Unk
      (let+ lhs = lhs and+ rhs = rhs in
       L.arith oper lhs rhs k)

  let neg (Unk expr) k =
    Unk
      (let+ expr = expr in
       L.neg expr k)

  let cmp rel (Unk lhs) (Unk rhs) k =
    Unk
      (let+ lhs = lhs and+ rhs = rhs in
       L.cmp rel lhs rhs k)

  let value target k =
    Unk
      (let+ target = unwrap target in
       L.value target k)

  let if_ ~condition:(Unk condition) ~yes:(Unk yes) ?no k =
    Unk
      (fun s ->
        let condition, s = condition s in
        let yes, s = yes s in
        let no, s =
          match no with
          | None -> (None, s)
          | Some (Unk no) ->
              let no, s = no s in
              (Some no, s)
          | _ -> assert false in
        (L.if_ ~condition ~yes ?no k, s) )

  let while_ ~condition:(Unk condition) ~body:(Unk body) k =
    Unk
      (let+ condition = condition and+ body = body in
       L.while_ ~condition ~body k)

  let for_ name ~initial:(Unk initial) ~final:(Unk final) ~body:(Unk body) k =
    Unk
      (fun s ->
        let initial, s = initial s in
        let final, s = final s in
        let env = Name_table.add name k s.env
        and mutability = Table.add k `Mutates s.mutability in
        let s0 = s in
        let s = {env; mutability} in
        let body, s = body s in
        (L.for_ name ~initial ~final ~body k, {s0 with mutability= s.mutability})
        )

  let break k = Unk (pure (L.break k))

  let seq exprs k =
    Unk
      (let+ exprs =
         traverse (function Unk expr -> expr | _ -> assert false) exprs
       in
       L.seq exprs k)

  let call name args k =
    Unk
      (let+ args =
         traverse (function Unk arg -> arg | _ -> assert false) args
       in
       L.call name args k)

  let uniform_array name ~size:(Unk size) ~initial:(Unk initial) k =
    Unk
      (let+ size = size and+ initial = initial in
       L.uniform_array name ~size ~initial k)

  let array name exprs k =
    Unk
      (let+ exprs =
         traverse (function Unk expr -> expr | _ -> assert false) exprs
       in
       L.array name exprs k)

  let size (Unk expr) k =
    Unk
      (let+ expr = expr in
       L.size expr k)

  let record name bindings k =
    Unk
      (let+ bindings =
         traverse
           (fun (field_name, expr) ->
             match expr with
             | Unk expr ->
                 let+ expr = expr in
                 (field_name, expr)
             | _ -> assert false )
           bindings
       in
       L.record name bindings k)

  let scope decls (Unk expr) k =
    Unk
      (fun s ->
        let s0 = s in
        let decls, s =
          (traverse (function Unk decl -> decl | _ -> assert false) decls) s
        in
        let expr, s = expr s in
        (L.scope decls expr k, {s0 with mutability= s.mutability}) )

  let assign target (Unk expr) k =
    Unk
      (let* expr = expr in
       let* s = get in
       let* () =
         match target with
         | Name (name, _) ->
             let k_var = Name_table.find name s.env in
             set {s with mutability= Table.add k_var `Mutates s.mutability}
         | Unk _ -> pure ()
       in
       let+ target = unwrap target in
       L.assign target expr k)

  let name name k = Name (name, L.name name k)

  let index target (Unk expr) k =
    Unk
      (let+ target = unwrap target and+ expr = expr in
       L.index target expr k)

  let access target name k =
    Unk
      (let+ target = unwrap target in
       L.access target name k)

  let var name ?type_name (Unk expr) k =
    Unk
      (fun s ->
        let expr, s = expr s in
        let env = Name_table.add name k s.env
        and mutability = Table.add k `Constant s.mutability in
        (L.var name ?type_name expr k, {env; mutability}) )

  let fns fs k =
    Unk
      (let+ fs = traverse (function Unk f -> f | _ -> assert false) fs in
       L.fns fs k)

  let types ts k =
    Unk
      (let+ ts = traverse (function Unk t -> t | _ -> assert false) ts in
       L.types ts k)

  let fn name params ?type_name (Unk body) k =
    Unk
      (let* params =
         traverse
           (fun (param, type_name) ->
             match param with
             | Unk param ->
                 let+ param = param in
                 (param, type_name)
             | _ -> assert false )
           params
       in
       let+ body = body in
       L.fn name params ?type_name body k)

  let param name k =
    Unk
      (fun s ->
        let env = Name_table.add name k s.env
        and mutability = Table.add k `Constant s.mutability in
        (L.param name k, {env; mutability}) )

  let alias_type ~name ~target k = Unk (pure (L.alias_type ~name ~target k))
  let array_type ~name ~item k = Unk (pure (L.array_type ~name ~item k))
  let record_type name fields k = Unk (pure (L.record_type name fields k))

  let table = function
    | Unk t ->
        let x, s = t {env= Name_table.empty; mutability= Table.empty} in
        (x, s.mutability)
    | _ -> assert false

  let analyze t =
    let x, mutability = table t in
    (mutability, x)
end
