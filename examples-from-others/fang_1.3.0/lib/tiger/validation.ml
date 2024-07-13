(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Prelude

module Ident : sig
  type t

  val v : string -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val string : t -> string
  val pp : t Fmt.t [@@ocaml.toplevel_printer]
end = struct
  module Table = Ephemeron.K1.Make (struct
    include String

    let hash = Hashtbl.hash
  end)

  let table = Table.create 64
  let next_index = ref 0

  type t = int * string

  let v s =
    match Table.find_opt table s with
    | Some x -> (x, s)
    | None ->
        let x = !next_index in
        incr next_index ; Table.add table s x ; (x, s)

  let equal (x1, _) (x2, _) = Int.equal x1 x2
  let compare (x1, _) (x2, _) = Int.compare x1 x2
  let string (_, s) = s
  let pp ppf (_, s) = Fmt.(styled (`Fg `Blue) string) ppf s
end

type ident = Ident.t

module Ident_set = Set.Make (Ident)
module Ident_map = Map.Make (Ident)

let unit_ident = Ident.v "unit"
let int_ident = Ident.v "int"
let string_ident = Ident.v "string"
let nil_ident = Ident.v "nil"

module Unique : sig
  type t

  val make : unit -> t
  val equal : t -> t -> bool
end = struct
  type t = unit ref

  let make () = ref ()
  let equal t1 t2 = t1 == t2
end

type unique = Unique.t

module Type = struct
  type t =
    | Unit
    | Int
    | String
    | Nil
    | Alias of ident * t
    | Array of ident * t * unique
    | Record of ident * (ident * t) list * unique
    | Named of ident * t option ref

  let rec iter_named f = function
    | Unit | Int | String | Nil -> ()
    | Alias (_, t) -> iter_named f t
    | Array (_, t, _) -> iter_named f t
    | Record (_, fields, _) -> List.iter (fun (_, t) -> iter_named f t) fields
    | Named (name, tref) -> f (name, tref)

  let rec kind = function
    | Unit -> `Unit
    | Int -> `Int
    | String -> `String
    | Nil -> `Nil
    | Alias (_, t) -> kind t
    | Array (_, t, unique) -> `Array (t, unique)
    | Record (_, fields, unique) -> `Record (fields, unique)
    | Named (_, tyref) -> kind (Option.get !tyref)

  let equal k1 k2 =
    match (k1, k2) with
    | `Unit, `Unit -> true
    | `Unit, _ -> false
    | `Int, `Int -> true
    | `Int, _ -> false
    | `String, `String -> true
    | `String, _ -> false
    | `Nil, `Nil -> true
    | `Nil, `Record _ -> true
    | `Record _, `Nil -> true
    | `Nil, _ -> false
    | `Array (_, u1), `Array (_, u2) -> Unique.equal u1 u2
    | `Array _, _ -> false
    | `Record (_, u1), `Record (_, u2) -> Unique.equal u1 u2
    | `Record _, _ -> false

  let name = function
    | Unit -> unit_ident
    | Int -> int_ident
    | String -> string_ident
    | Nil -> nil_ident
    | Alias (name, _) -> name
    | Array (name, _, _) -> name
    | Record (name, _, _) -> name
    | Named (name, _) -> name

  let has_illegal_cycle = function
    | Alias (_, t) -> (
        let ancestors = Hashtbl.create 4 in
        let exception Cycle in
        let rec loop path t =
          Hashtbl.add ancestors t () ;
          let path =
            match t with Array _ | Record _ -> t :: path | _ -> path in
          iter_named
            (fun (_, tyref) ->
              let t_next = Option.get !tyref in
              if Hashtbl.mem ancestors t_next then
                match (t_next, path) with
                | Alias _, [] -> raise_notrace Cycle
                | _ -> ()
              else loop path t_next )
            t in
        try loop [] t ; false with Cycle -> true )
    | _ -> false
end

let unzip xs =
  let rec cps xs k =
    match xs with
    | [] -> k ([], [])
    | (y, z) :: xs -> cps xs (fun (ys, zs) -> k (y :: ys, z :: zs)) in
  cps xs Fun.id

let unzip3 zs =
  let rec cps zs k =
    match zs with
    | [] -> k ([], [], [])
    | (w, x, y) :: zs ->
        cps zs (fun (ws, xs, ys) -> k (w :: ws, x :: xs, y :: ys)) in
  cps zs Fun.id

type error = unit Fmt.t

module Make (L : TIGER) = struct
  type _ t =
    | Expr : (expr L.t * Type.t * Source_span.t) repr -> expr t
    | Target : (target L.t * Type.t * Source_span.t) repr -> target t
    | Decl : decl L.t repr -> decl t
    | Fn : (ident * (expr L.t -> fn L.t)) repr -> fn t
    | Param : (param L.t * ident * Source_span.t) repr -> param t
    | Typ : typ L.t repr -> typ t

  and fundef = Built_in of Type.t list * Type.t | User_defined of ufn

  and ufn =
    { mutable ufn_state: state
    ; ufn_params: (ident * Type.t * Source_span.t) list
    ; ufn_ty: Type.t
    ; ufn_k: Source_span.t
    ; ufn_body: expr t }

  and state =
    { loop_depth: int
    ; vars: Type.t Ident_map.t
    ; unresolved_types: (Type.t * Source_span.t) Ident_map.t
    ; types: (Type.t * Source_span.t) Ident_map.t
    ; unresolved_fundefs: ufn Ident_map.t
    ; fundefs: fundef Ident_map.t }

  and 'a repr = state -> ('a * state, error * Source_span.t) result

  let pure x : 'a repr = fun s -> Ok (x, s)

  let error : Source_span.t -> unit Fmt.t -> 'a repr =
   fun k pp _ -> Error (pp, k)

  let get : state repr = fun s -> Ok (s, s)
  let set : state -> unit repr = fun s _ -> Ok ((), s)

  let map (f : 'a -> 'b) (r : 'a repr) : 'b repr =
   fun s -> match r s with Error e -> Error e | Ok (x, s) -> Ok (f x, s)

  let bind (f : 'a -> 'b repr) (r : 'a repr) : 'b repr =
   fun s -> match r s with Error e -> Error e | Ok (x, s) -> (f x) s

  let ( let* ) r f = bind f r
  let ( let+ ) r f = map f r

  let ( and+ ) (r1 : 'a repr) (r2 : 'b repr) : ('a * 'b) repr =
    let* x1 = r1 in
    let* x2 = r2 in
    pure (x1, x2)

  let traverse (f : 'a -> 'b repr) (xs : 'a list) : 'b list repr =
    let rec cps xs k =
      match xs with
      | [] -> k (pure [])
      | x :: xs ->
          let* y = f x in
          cps xs (fun ys ->
              let* ys = ys in
              k (pure (y :: ys)) ) in
    cps xs Fun.id

  let as_expr (Expr r) = r
  let as_target (Target r) = r
  let as_decl (Decl r) = r
  let as_fn (Fn r) = r
  let as_param (Param r) = r
  let as_typ (Typ r) = r

  let pp_describe_type k ppf = function
    | Type.Int ->
        Fmt.(
          box (const words "the built-in type" ++ sp ++ const Ident.pp int_ident))
          ppf ()
    | String ->
        Fmt.(
          box
            ( const words "the built-in type"
            ++ sp
            ++ const Ident.pp string_ident ))
          ppf ()
    | Unit | Nil | Named _ -> assert false
    | Alias _ ->
        Fmt.(
          box (const words "an alias defined at" ++ sp ++ const Source_span.pp k))
          ppf ()
    | Array _ ->
        Fmt.(
          box (const words "an array defined at" ++ sp ++ const Source_span.pp k))
          ppf ()
    | Record _ ->
        Fmt.(
          box (const words "a record defined at" ++ sp ++ const Source_span.pp k))
          ppf ()

  let error_wrong_type ~expected ~actual k =
    error k
      Fmt.(
        box
          ( const words "Expected a value of type"
          ++ sp
          ++ const Ident.pp (Type.name expected)
          ++ sp
          ++ const words "but this is a value of type"
          ++ sp
          ++ const Ident.pp (Type.name actual) ))

  let int x k = Expr (pure (L.int x k, Type.Int, k))
  let string s k = Expr (pure (L.string s k, Type.String, k))
  let nil k = Expr (pure (L.nil k, Type.Nil, k))

  let arith oper lhs rhs k =
    Expr
      (let* lhs, ty_lhs, k_lhs = as_expr lhs in
       let* () =
         match Type.kind ty_lhs with
         | `Int -> pure ()
         | _ -> error_wrong_type ~expected:Type.Int ~actual:ty_lhs k_lhs
       in
       let* rhs, ty_rhs, k_rhs = as_expr rhs in
       let* () =
         match Type.kind ty_rhs with
         | `Int -> pure ()
         | _ -> error_wrong_type ~expected:Type.Int ~actual:ty_rhs k_rhs
       in
       pure (L.arith oper lhs rhs k, Type.Int, k))

  let neg expr k =
    Expr
      (let* expr, ty, k_expr = as_expr expr in
       match Type.kind ty with
       | `Int -> pure (L.neg expr k, ty, k)
       | _ -> error_wrong_type ~expected:Type.Int ~actual:ty k_expr)

  let cmp (rel : rel) lhs rhs k =
    Expr
      (let* lhs, ty_lhs, _k_lhs = as_expr lhs in
       let* rhs, ty_rhs, _k_rhs = as_expr rhs in
       let tykind_lhs = Type.kind ty_lhs and tykind_rhs = Type.kind ty_rhs in
       if not (Type.equal tykind_lhs tykind_rhs) then
         error k
           Fmt.(
             box
               ( const words "Cannot compare these incompatible values of type"
               ++ sp
               ++ const Ident.pp (Type.name ty_lhs)
               ++ sp ++ const string "and" ++ sp
               ++ const Ident.pp (Type.name ty_rhs) ))
       else
         match (rel, tykind_lhs, tykind_rhs) with
         | _, `Int, `Int
          |_, `String, `String
          |(`Equal | `Not_equal), `Unit, `Unit
          |(`Equal | `Not_equal), `Array _, `Array _
          |(`Equal | `Not_equal), (`Record _ | `Nil), (`Record _ | `Nil) ->
             pure (L.cmp rel lhs rhs k, Type.Int, k)
         | _, (`Record _ | `Nil), _ ->
             error k
               Fmt.(
                 box
                   (const words
                      "Cannot compare these record values. Record values can \
                       only be compared for equality" ))
         | _, `Array _, _ ->
             error k
               Fmt.(
                 box
                   (const words
                      "Cannot compare these array values. Array values can \
                       only be compared for equality" ))
         | _, `Unit, _ ->
             error k
               Fmt.(
                 box
                   (const words
                      "Cannot compare these unit values. Unit values can only \
                       be compared for equality" ))
         | _ -> assert false)

  let value target k =
    Expr
      (let* target, ty_target, _ = as_target target in
       pure (L.value target k, ty_target, k))

  let if_ ~condition ~yes ?no k =
    Expr
      (let* condition, ty_condition, k_condition = as_expr condition in
       let* () =
         match Type.kind ty_condition with
         | `Int -> pure ()
         | _ ->
             error_wrong_type ~expected:Type.Int ~actual:ty_condition
               k_condition
       in
       let* yes, ty_yes, k_yes = as_expr yes in
       match (no, Type.kind ty_yes) with
       | None, `Unit -> pure (L.if_ ~condition ~yes k, ty_yes, k)
       | None, _ -> error_wrong_type ~expected:Type.Unit ~actual:ty_yes k_yes
       | Some no, tykind_yes ->
           let* no, ty_no, k_no = as_expr no in
           if not (Type.equal (Type.kind ty_no) tykind_yes) then
             error_wrong_type ~expected:ty_yes ~actual:ty_no k_no
           else pure (L.if_ ~condition ~yes ~no k, ty_yes, k))

  let while_ ~condition ~body k =
    Expr
      (let* condition, ty_condition, k_condition = as_expr condition in
       match Type.kind ty_condition with
       | `Int -> (
           let* s = get in
           let* () = set {s with loop_depth= s.loop_depth + 1} in
           let* body, ty_body, k_body = as_expr body in
           let* () = set s in
           match Type.kind ty_body with
           | `Unit -> pure (L.while_ ~condition ~body k, Type.Unit, k)
           | _ -> error_wrong_type ~expected:Type.Unit ~actual:ty_body k_body )
       | _ ->
           error_wrong_type ~expected:Type.Int ~actual:ty_condition k_condition)

  let for_ name ~initial ~final ~body k =
    Expr
      (let name = Ident.v name in
       let* initial, ty_initial, k_initial = as_expr initial in
       let* () =
         match Type.kind ty_initial with
         | `Int -> pure ()
         | _ -> error_wrong_type ~expected:Type.Int ~actual:ty_initial k_initial
       in
       let* final, ty_final, k_final = as_expr final in
       let* () =
         match Type.kind ty_final with
         | `Int -> pure ()
         | _ -> error_wrong_type ~expected:Type.Int ~actual:ty_final k_final
       in
       let* s = get in
       let vars = Ident_map.add name Type.Int s.vars in
       let* () = set {s with vars; loop_depth= s.loop_depth + 1} in
       let* body, ty_body, k_body = as_expr body in
       let* () =
         match Type.kind ty_body with
         | `Unit -> pure ()
         | _ -> error_wrong_type ~expected:Type.Unit ~actual:ty_body k_body
       in
       let* () = set s in
       pure (L.for_ (Ident.string name) ~initial ~final ~body k, Type.Unit, k)
      )

  let break k =
    Expr
      (let* s = get in
       if s.loop_depth <= 0 then
         error k
           Fmt.(
             box
               ( const (quote string) "break"
               ++ sp
               ++ const words "cannot appear outside of looping contexts" ))
       else pure (L.break k, Type.Unit, k))

  let seq exprs k =
    Expr
      (let* exprs, tys, _ = map unzip3 (traverse as_expr exprs) in
       let ty = match List.rev tys with [] -> Type.Unit | ty :: _ -> ty in
       pure (L.seq exprs k, ty, k))

  let pp_plural count ppf s =
    if count = 0 || count > 1 then Fmt.(string ++ const string "s") ppf s
    else Fmt.string ppf s

  let call_args name params args k =
    let* () =
      let num_params = List.length params and num_args = List.length args in
      if num_params <> num_args then
        error k
          Fmt.(
            box
              ( const words "The function" ++ sp ++ const Ident.pp name ++ sp
              ++ const string "expects" ++ sp ++ const int num_params ++ sp
              ++ const (pp_plural num_params) "argument"
              ++ sp
              ++ const words "but has been invoked with"
              ++ sp ++ const int num_args ))
      else pure ()
    in
    traverse
      (fun (arg, ty_param) ->
        let* arg, ty_arg, k_arg = as_expr arg in
        if not (Type.equal (Type.kind ty_arg) (Type.kind ty_param)) then
          error_wrong_type ~expected:ty_param ~actual:ty_arg k_arg
        else pure arg )
      (List.combine args params)

  let call name args k =
    Expr
      (let name = Ident.v name in
       let* s = get in
       let* args, ty =
         match Ident_map.find_opt name s.fundefs with
         | None ->
             error k
               Fmt.(
                 box
                   ( const words "Undefined function"
                   ++ sp ++ const Ident.pp name ))
         | Some (Built_in (params, ty)) ->
             let* args = call_args name params args k in
             pure (args, ty)
         | Some (User_defined ufn) ->
             let params = List.map (fun (_, ty, _) -> ty) ufn.ufn_params in
             let* args = call_args name params args k in
             pure (args, ufn.ufn_ty)
       in
       pure (L.call (Ident.string name) args k, ty, k) )

  let error_undefined_type name k =
    error k
      Fmt.(
        box
          ( const words "The type" ++ sp ++ const Ident.pp name ++ sp
          ++ const words "is not defined" ))

  let error_not_an_array ~k_ty name ty k =
    error k
      Fmt.(
        box
          ( const words "Cannot create an array value of type"
          ++ sp ++ const Ident.pp name ++ sp
          ++ const words "since that type is not defined as an array (it's"
          ++ sp
          ++ const (pp_describe_type k_ty) ty
          ++ const string ")" ))

  let uniform_array name ~size ~initial k =
    Expr
      (let name = Ident.v name in
       let* s = get in
       let* ty, k_ty =
         match Ident_map.find_opt name s.types with
         | None -> error_undefined_type name k
         | Some pair -> pure pair
       in
       match Type.kind ty with
       | `Array (ty_item, _) -> (
           let* size, ty_size, k_size = as_expr size in
           match Type.kind ty_size with
           | `Int ->
               let* initial, ty_initial, k_initial = as_expr initial in
               if not (Type.equal (Type.kind ty_initial) (Type.kind ty_item))
               then
                 error_wrong_type ~expected:ty_item ~actual:ty_initial k_initial
               else
                 pure
                   (L.uniform_array (Ident.string name) ~size ~initial k, ty, k)
           | _ -> error_wrong_type ~expected:Type.Int ~actual:ty_size k_size )
       | _ -> error_not_an_array ~k_ty name ty k )

  let array name exprs k =
    Expr
      (let name = Ident.v name in
       let* s = get in
       let* ty, k_ty =
         match Ident_map.find_opt name s.types with
         | None -> error_undefined_type name k
         | Some pair -> pure pair
       in
       match Type.kind ty with
       | `Array (ty_item, _) ->
           let* exprs =
             traverse
               (fun expr ->
                 let* expr, ty_expr, k_expr = as_expr expr in
                 if not (Type.equal (Type.kind ty_item) (Type.kind ty_expr))
                 then error_wrong_type ~expected:ty_item ~actual:ty_expr k_expr
                 else pure expr )
               exprs
           in
           pure (L.array (Ident.string name) exprs k, ty, k)
       | _ -> error_not_an_array ~k_ty name ty k )

  let size expr k =
    Expr
      (let* arg, ty_arg, k_arg = as_expr expr in
       match Type.kind ty_arg with
       | `Array (_, _) -> pure (L.size arg k, Type.Int, k)
       | _ ->
           error k_arg
             Fmt.(
               box
                 ( const words
                     "Cannot compute the size of this non-array value of type"
                 ++ sp
                 ++ const Ident.pp (Type.name ty_arg) )))

  let record_bindings name fields bindings k =
    let bindings =
      List.map (fun (field_name, expr) -> (Ident.v field_name, expr)) bindings
    in
    let* () =
      let exception Duplicate of ident in
      let sorted_binding_names =
        List.map fst bindings |> List.sort Ident.compare in
      match sorted_binding_names with
      | [] | [_] -> pure ()
      | name :: names -> (
          let prev = ref name in
          try
            List.iter
              (fun name ->
                if Ident.equal name !prev then raise_notrace (Duplicate name)
                else prev := name )
              names ;
            pure ()
          with Duplicate duplicated_name ->
            error k
              Fmt.(
                box
                  ( const words "Cannot assign a value to the record field"
                  ++ sp
                  ++ const Ident.pp duplicated_name
                  ++ sp
                  ++ const words "multiple times" )) )
    in
    let binding_names =
      List.fold_right Ident_set.add (List.map fst bindings) Ident_set.empty
    and field_names =
      List.fold_right Ident_set.add (List.map fst fields) Ident_set.empty in
    let extra_names =
      Ident_set.diff binding_names field_names |> Ident_set.elements
    and missing_names =
      Ident_set.diff field_names binding_names |> Ident_set.elements in
    let* () =
      match extra_names with
      | [] -> pure ()
      | extra_names ->
          error k
            Fmt.(
              box
                ( const words
                    "Cannot assign values to these fields of the record type"
                ++ sp ++ const Ident.pp name ++ sp
                ++ const words "since they don't exist:"
                ++ sp
                ++ const (box (list ~sep:comma Ident.pp)) extra_names ))
    in
    let* () =
      match missing_names with
      | [] -> pure ()
      | missing_names ->
          error k
            Fmt.(
              box
                ( const words
                    "Missing assignments to these fields of the record type"
                ++ sp ++ const Ident.pp name ++ const string ":" ++ sp
                ++ const (box (list ~sep:comma Ident.pp)) missing_names ))
    in
    traverse
      (fun (field_name, expr) ->
        let* expr, ty_expr, k_expr = as_expr expr in
        let ty_field = List.assoc field_name fields in
        if not (Type.equal (Type.kind ty_expr) (Type.kind ty_field)) then
          error_wrong_type ~expected:ty_field ~actual:ty_expr k_expr
        else pure (field_name, expr) )
      bindings

  let record name bindings k =
    Expr
      (let name = Ident.v name in
       let* s = get in
       let* ty, fields =
         match Ident_map.find_opt name s.types with
         | None -> error_undefined_type name k
         | Some (ty, k_ty) -> (
           match Type.kind ty with
           | `Record (fields, _) -> pure (ty, fields)
           | _ ->
               error k
                 Fmt.(
                   box
                     ( const words "Cannot create a record value of type"
                     ++ sp ++ const Ident.pp name ++ sp
                     ++ const words
                          "since that type is not defined as a record (it's"
                     ++ sp
                     ++ const (pp_describe_type k_ty) ty
                     ++ const string ")" )) )
       in
       let* bindings = record_bindings name fields bindings k in
       pure
         ( L.record (Ident.string name)
             (List.map
                (fun (field_name, expr) -> (Ident.string field_name, expr))
                bindings )
             k
         , ty
         , k ) )

  let scope decls expr k =
    Expr
      (let* s = get in
       let* decls = traverse as_decl decls in
       let* expr, ty, _ = as_expr expr in
       let* () = set s in
       pure (L.scope decls expr k, ty, k))

  let assign target expr k =
    Expr
      (let* target, ty_target, _ = as_target target in
       let* expr, ty_expr, k_expr = as_expr expr in
       if not (Type.equal (Type.kind ty_target) (Type.kind ty_expr)) then
         error_wrong_type ~expected:ty_target ~actual:ty_expr k_expr
       else pure (L.assign target expr k, Type.Unit, k))

  let name name k =
    Target
      (let name = Ident.v name in
       let* s = get in
       match Ident_map.find_opt name s.vars with
       | None ->
           error k
             Fmt.(
               box
                 (const words "Undefined variable" ++ sp ++ const Ident.pp name))
       | Some ty -> pure (L.name (Ident.string name) k, ty, k) )

  let index target expr k =
    Target
      (let* target, ty_target, k_target = as_target target in
       let* ty_item =
         match Type.kind ty_target with
         | `Array (ty_item, _) -> pure ty_item
         | _ ->
             error k_target
               Fmt.(
                 box
                   ( const words "Cannot index a value of non-array type"
                   ++ sp
                   ++ const Ident.pp (Type.name ty_target) ))
       in
       let* expr, ty_expr, k_expr = as_expr expr in
       match Type.kind ty_expr with
       | `Int -> pure (L.index target expr k, ty_item, k)
       | _ -> error_wrong_type ~expected:Type.Int ~actual:ty_expr k_expr)

  let access target name k =
    Target
      (let name = Ident.v name in
       let* target, ty_target, k_target = as_target target in
       match Type.kind ty_target with
       | `Record (fields, _) -> (
         match List.assoc_opt name fields with
         | None ->
             error k
               Fmt.(
                 box
                   ( const words "The record type"
                   ++ sp
                   ++ const Ident.pp (Type.name ty_target)
                   ++ sp ++ const words "has no field" ++ sp
                   ++ const Ident.pp name ))
         | Some ty_field ->
             pure (L.access target (Ident.string name) k, ty_field, k) )
       | _ ->
           error k_target
             Fmt.(
               box
                 ( const words "Cannot access the field"
                 ++ sp ++ const Ident.pp name ++ sp
                 ++ const words "of a value of non-record type"
                 ++ sp
                 ++ const Ident.pp (Type.name ty_target) )) )

  let var name ?type_name expr k =
    Decl
      (let name = Ident.v name in
       let* expr, ty_expr, k_expr = as_expr expr in
       let* s = get in
       let* ty =
         match type_name with
         | None -> (
           match Type.kind ty_expr with
           | `Nil ->
               error k
                 Fmt.(
                   box
                     ( const Ident.pp name ++ sp
                     ++ const words "cannot be assigned the value"
                     ++ sp ++ const Ident.pp nil_ident ++ sp
                     ++ const words
                          "without its type being constrained to a particular \
                           record type" ))
           | _ -> pure ty_expr )
         | Some type_name -> (
             let type_name = Ident.v type_name in
             match Ident_map.find_opt type_name s.types with
             | None -> error_undefined_type type_name k
             | Some (ty_constraint, _) ->
                 if
                   not
                     (Type.equal (Type.kind ty_expr) (Type.kind ty_constraint))
                 then
                   error_wrong_type ~expected:ty_constraint ~actual:ty_expr
                     k_expr
                 else pure ty_constraint )
       in
       let vars = Ident_map.add name ty s.vars in
       let* () = set {s with vars} in
       pure (L.var (Ident.string name) ?type_name expr k) )

  let fns fs k =
    Decl
      (let* pairs =
         traverse
           (fun f ->
             let* name, f = as_fn f in
             pure (name, f) )
           fs
       in
       let names = List.map fst pairs in
       let* () =
         let exception Repeated of ident in
         match List.sort Ident.compare names with
         | [] | [_] -> pure ()
         | name :: names -> (
             let prev = ref name in
             try
               List.iter
                 (fun name ->
                   if Ident.equal name !prev then raise (Repeated name)
                   else prev := name )
                 names ;
               pure ()
             with Repeated name ->
               error k
                 Fmt.(
                   box
                     ( const words "The function" ++ sp ++ const Ident.pp name
                     ++ sp
                     ++ const words
                          "cannot be defined multiple times in this group of \
                           mutually-recursive function definitions" )) )
       in
       let* s0 = get in
       let* fs =
         traverse
           (fun (name, f) ->
             let ufn = Ident_map.find name s0.unresolved_fundefs in
             let s =
               List.fold_right
                 (fun (param_name, ty_param, _) s ->
                   let vars = Ident_map.add param_name ty_param s.vars in
                   {s with vars} )
                 ufn.ufn_params ufn.ufn_state in
             let s =
               List.fold_right
                 (fun name s ->
                   let ufn = Ident_map.find name s0.unresolved_fundefs in
                   let fundefs =
                     Ident_map.add name (User_defined ufn) s.fundefs in
                   {s with fundefs} )
                 names s in
             let* () = set s in
             let* body, ty_body, k_body = as_expr ufn.ufn_body in
             let* () =
               if not (Type.equal (Type.kind ty_body) (Type.kind ufn.ufn_ty))
               then error_wrong_type ~expected:ufn.ufn_ty ~actual:ty_body k_body
               else pure ()
             in
             let* () = set s0 in
             pure (f body) )
           pairs
       in
       let s =
         List.fold_right
           (fun name s ->
             let ufn = Ident_map.find name s.unresolved_fundefs in
             let unresolved_fundefs = Ident_map.remove name s.unresolved_fundefs
             and fundefs = Ident_map.add name (User_defined ufn) s.fundefs in
             {s with unresolved_fundefs; fundefs} )
           names s0 in
       let* () = set s in
       pure (L.fns fs k))

  let resolve_types =
    let* s = get in
    Ident_map.iter
      (fun _ (ty, _) ->
        Type.iter_named
          (fun (name_to_resolve, ty_ref) ->
            match Ident_map.find_opt name_to_resolve s.unresolved_types with
            | Some (ty, _) -> ty_ref := Some ty
            | None -> (
              match Ident_map.find_opt name_to_resolve s.types with
              | Some (ty, _) -> ty_ref := Some ty
              | None -> () ) )
          ty )
      s.unresolved_types ;
    let exception Unresolved of Ident.t * Source_span.t in
    try
      Ident_map.iter
        (fun _ty_name (ty, k_ty) ->
          Type.iter_named
            (fun (name_to_resolve, ty_ref) ->
              match !ty_ref with
              | None -> raise (Unresolved (name_to_resolve, k_ty))
              | Some _ -> () )
            ty )
        s.unresolved_types ;
      pure ()
    with Unresolved (name, k) ->
      error k
        Fmt.(box (const words "Undefined type" ++ sp ++ const Ident.pp name))

  let check_for_illegal_cycles k =
    let exception Cycle in
    let* s = get in
    try
      Ident_map.iter
        (fun _ (ty, _) -> if Type.has_illegal_cycle ty then raise_notrace Cycle)
        s.unresolved_types ;
      pure ()
    with Cycle ->
      error k
        Fmt.(
          box
            (const words
               "A cycle of mutually-recursive type definitions must pass \
                through either a record or array type" ))

  let types ts k =
    Decl
      (let* ts = traverse as_typ ts in
       let* () = resolve_types in
       let* () = check_for_illegal_cycles k in
       let* s = get in
       let types =
         Ident_map.union (fun _ _ ty -> Some ty) s.types s.unresolved_types
       and unresolved_types = Ident_map.empty in
       let* () = set {s with types; unresolved_types} in
       pure (L.types ts k))

  let fn name pairs ?type_name body k =
    Fn
      (let name = Ident.v name in
       let* s = get in
       let observed_param_names = ref Ident_set.empty in
       let* params =
         traverse
           (fun (param, param_type_name) ->
             let param_type_name = Ident.v param_type_name in
             let* ty_param =
               match Ident_map.find_opt param_type_name s.types with
               | None -> error_undefined_type param_type_name k
               | Some (ty, _) -> pure ty
             in
             let* param, param_ident, k_param = as_param param in
             if Ident_set.mem param_ident !observed_param_names then
               error k_param
                 Fmt.(
                   box
                     ( const words
                         "There cannot be multiple parameters with the name"
                     ++ sp ++ const Ident.pp param_ident ++ sp
                     ++ const words "in the definition of"
                     ++ sp ++ const Ident.pp name ))
             else (
               observed_param_names :=
                 Ident_set.add param_ident !observed_param_names ;
               pure (param, param_ident, ty_param, k_param) ) )
           pairs
       in
       let* ufn_ty =
         match type_name with
         | None -> pure Type.Unit
         | Some type_name -> (
             let type_name = Ident.v type_name in
             match Ident_map.find_opt type_name s.types with
             | None -> error_undefined_type type_name k
             | Some (ty, _) -> pure ty )
       in
       let ufn_params =
         List.map
           (fun (_, param_ident, ty_param, k_param) ->
             (param_ident, ty_param, k_param) )
           params in
       let ufn = {ufn_state= s; ufn_params; ufn_ty; ufn_k= k; ufn_body= body} in
       let unresolved_fundefs = Ident_map.add name ufn s.unresolved_fundefs in
       let* () = set {s with unresolved_fundefs} in
       let pairs =
         List.map
           (fun (param, _, ty_param, _) ->
             (param, Ident.string (Type.name ty_param)) )
           params in
       pure (name, fun body -> L.fn (Ident.string name) pairs ?type_name body k)
      )

  let param name k =
    Param
      (let name_ident = Ident.v name in
       pure (L.param name k, name_ident, k) )

  let error_type_defined_more_than_once (ty_prev, k_prev) name k =
    error k
      Fmt.(
        box
          ( const words "The type" ++ sp ++ const Ident.pp name ++ sp
          ++ const words
               "cannot be defined more than once in this group of type \
                definitions (it's already"
          ++ sp
          ++ const (pp_describe_type k_prev) ty_prev
          ++ const string ")" ))

  let alias_type ~name ~target k =
    Typ
      (let name = Ident.v name and target = Ident.v target in
       let* s = get in
       match Ident_map.find_opt name s.unresolved_types with
       | Some prev -> error_type_defined_more_than_once prev name k
       | None ->
           let ty = Type.Alias (name, Named (target, ref None)) in
           let unresolved_types =
             Ident_map.add name (ty, k) s.unresolved_types in
           let* () = set {s with unresolved_types} in
           pure
             (L.alias_type ~name:(Ident.string name)
                ~target:(Ident.string target) k ) )

  let array_type ~name ~item k =
    Typ
      (let name = Ident.v name and item = Ident.v item in
       let* s = get in
       match Ident_map.find_opt name s.unresolved_types with
       | Some prev -> error_type_defined_more_than_once prev name k
       | None ->
           let ty = Type.Array (name, Named (item, ref None), Unique.make ()) in
           let unresolved_types =
             Ident_map.add name (ty, k) s.unresolved_types in
           let* () = set {s with unresolved_types} in
           pure
             (L.array_type ~name:(Ident.string name) ~item:(Ident.string item) k)
      )

  let record_type_fields record_name fields k =
    let observed_names = ref [] in
    traverse
      (fun (name, type_name) ->
        let name = Ident.v name in
        if List.exists (Ident.equal name) !observed_names then
          error k
            Fmt.(
              box
                ( const words "There cannot be multiple fields with the name"
                ++ sp ++ const Ident.pp name ++ sp
                ++ const words "in the definition of the"
                ++ sp ++ const Ident.pp record_name ++ sp
                ++ const string "record" ))
        else (
          observed_names := name :: !observed_names ;
          let ty = Type.Named (Ident.v type_name, ref None) in
          pure (name, ty) ) )
      fields

  let record_type name fields k =
    Typ
      (let fields0 = fields in
       let name = Ident.v name in
       let* s = get in
       match Ident_map.find_opt name s.unresolved_types with
       | Some prev -> error_type_defined_more_than_once prev name k
       | None ->
           let* fields = record_type_fields name fields k in
           let ty = Type.Record (name, fields, Unique.make ()) in
           let unresolved_types =
             Ident_map.add name (ty, k) s.unresolved_types in
           let* () = set {s with unresolved_types} in
           pure (L.record_type (Ident.string name) fields0 k) )

  let validate t =
    let s =
      let k_dummy = Source_span.v (0, 0) (0, 0) in
      { loop_depth= 0
      ; vars= Ident_map.empty
      ; unresolved_types= Ident_map.empty
      ; types=
          Ident_map.(
            empty
            |> add int_ident (Type.Int, k_dummy)
            |> add string_ident (Type.String, k_dummy))
      ; unresolved_fundefs= Ident_map.empty
      ; fundefs=
          Ident_map.(
            empty
            |> add (Ident.v "chr") (Built_in ([Type.Int], Type.String))
            |> add (Ident.v "concat")
                 (Built_in ([Type.String; Type.String], Type.String))
            |> add (Ident.v "exit") (Built_in ([Type.Int], Type.Unit))
            |> add (Ident.v "flush") (Built_in ([], Type.Unit))
            |> add (Ident.v "read_char") (Built_in ([], Type.String))
            |> add (Ident.v "not") (Built_in ([Type.Int], Type.Int))
            |> add (Ident.v "ord") (Built_in ([Type.String], Type.Int))
            |> add (Ident.v "print") (Built_in ([Type.String], Type.Unit))
            |> add (Ident.v "print_int") (Built_in ([Type.Int], Type.Unit))
            |> add (Ident.v "print_line") (Built_in ([], Type.Unit))
            |> add (Ident.v "len") (Built_in ([Type.String], Type.Int))
            |> add (Ident.v "substring")
                 (Built_in ([Type.String; Type.Int; Type.Int], Type.String))
            |> add (Ident.v "random") (Built_in ([Type.Int; Type.Int], Type.Int))
            |> add (Ident.v "seed") (Built_in ([Type.Int], Type.Unit))
            |> add (Ident.v "error") (Built_in ([Type.String], Type.Unit))) }
    in
    match (as_expr t) s with
    | Error e -> Error e
    | Ok ((next_t, _, _), _) -> Ok next_t
end
