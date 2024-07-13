(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_frame
open Fang_ir
open Fang_iter
open Fang_monads
open Fang_tiger
open Fang_vm

type 'a fragment = label * 'a

module type TRANSLATE = sig
  type _ t
  type _ ir

  val translate :
       ?safe:bool
    -> analysis
    -> expr t
    -> (value ir * frame_ptr) fragment iter * string fragment iter

  include TIGER with type 'a t := 'a t
end

module String_set = Set.Make (String)
module String_map = Map.Make (String)
module Label_map = Map.Make (Label)

module Type : sig
  type t

  val unit : t
  val int : t
  val string : t
  val nil : t
  val record : (string * t) list -> t
  val alias : t -> t
  val array : t -> t
  val named : string -> t

  type direct =
    [`Unit | `Int | `String | `Record of (string * t) list | `Array of t]

  val direct : t -> direct
  val resolve : (string -> t) -> t -> unit
end = struct
  type t =
    | Unit
    | Int
    | String
    | Record of (string * t) list
    | Alias of t
    | Array of t
    | Named of string * t option ref

  let unit = Unit
  let int = Int
  let string = String
  let nil = Record []
  let record fields = Record fields
  let alias t = Alias t
  let array t = Array t
  let named name = Named (name, ref None)

  type direct =
    [`Unit | `Int | `String | `Record of (string * t) list | `Array of t]

  let rec direct = function
    | Unit -> `Unit
    | Int -> `Int
    | String -> `String
    | Record fields -> `Record fields
    | Alias t -> direct t
    | Array t -> `Array t
    | Named (_, tref) -> direct (Option.get !tref)

  let rec iter_named f = function
    | Unit | Int | String -> ()
    | Record fields -> List.iter (fun (_, t) -> iter_named f t) fields
    | Alias t -> iter_named f t
    | Array t -> iter_named f t
    | Named (name, tref) -> f (name, tref)

  let resolve assoc t =
    iter_named
      (fun (name_to_resolve, tref) ->
        match !tref with
        | Some _ -> ()
        | None -> tref := Some (assoc name_to_resolve) )
      t
end

type knowledge =
  | Unknown
  | Known_nil
  | Known_non_nil
  | Known_int of int64
  | Known_size of int64

let join_knowledge w1 w2 = if w1 = w2 then w1 else Unknown

module Translate (F : FRAME) (I : IR with type 'a t = 'a F.ir) :
  TRANSLATE with type 'a ir = 'a I.t = struct
  type 'a ir = 'a I.t

  (** Every Tiger function (including nested functions) results in an IR frame.
      Each such frame needs a unique name. However, functions can be redefined.
      For example,

      {v
let
  function adder(x: int): int = x + 1
  var v1 := adder(3)
  function adder(x: int): int = x + 2
  var v2 := adder(3)
in
  ()
end
      v}

      If we blindly used the names [Fang.adder] for both functions then the
      names would conflict. Instead, we ensure conflicting names are distinct by
      appending a suffix. In this example, the two frame names would be
      [Fang.adder] and [Fang.adder_1]. *)
  module Frame_namer : sig
    type t

    val create : unit -> t
    val distinct : t -> F.t -> string -> string
  end = struct
    type t = (string, int) Hashtbl.t

    let create () = Hashtbl.create 4
    let qualify parent_name base_name = parent_name ^ "." ^ base_name

    let distinct t parent_frame base_name =
      let qualified_name = qualify (F.name parent_frame) base_name in
      match Hashtbl.find_opt t qualified_name with
      | None ->
          Hashtbl.add t qualified_name 1 ;
          qualified_name
      | Some usage_count ->
          Hashtbl.replace t qualified_name (usage_count + 1) ;
          qualified_name ^ "_" ^ string_of_int usage_count
  end

  module Level : sig
    type t
    type handle

    val outermost : t

    val make :
         parent:t
      -> Frame_namer.t
      -> string
      -> scope list
      -> [`Branch | `Leaf]
      -> t

    val parent : t -> t
    val alloc : scope -> t -> handle * t
    val depth : t -> int
    val args : t -> int -> handle
    val access : current:t -> handle -> value ir
    val static_link : address:value ir -> t -> value ir
    val frame : t -> F.t
    val label : t -> label
  end = struct
    type t = {frame: F.t; depth: int; parent: t option; id: unit ref}
    type handle = t * F.handle

    let outermost = {frame= F.make "fang" []; depth= 1; parent= None; id= ref ()}
    let parent t = match t.parent with None -> outermost | Some t -> t

    let make ~parent namer base_name args reach =
      let name = Frame_namer.distinct namer parent.frame base_name in
      let scope_of_static_link =
        match reach with `Branch -> `Escapes | `Leaf -> `Local in
      { frame= F.make name (scope_of_static_link :: args)
      ; depth= parent.depth + 1
      ; parent= Some parent
      ; id= ref () }

    let depth t = t.depth

    let alloc scope t =
      let h, frame = F.alloc scope t.frame in
      let t = {t with frame} in
      ((t, h), t)

    let static_link ~address t =
      match t.parent with
      | None -> F.address
      | Some _ -> F.access ~address (F.args t.frame 0)

    let equal t1 t2 = t1.id == t2.id

    let access ~current:t (t_defn, h) =
      let address =
        let rec loop address t =
          if equal t t_defn then address
          else loop (static_link ~address t) (Option.get t.parent) in
        loop F.address t in
      F.access ~address h

    let args t i = (t, F.args t.frame (i + 1))
    let frame t = t.frame
    let label t = F.label t.frame
  end

  type fundef = Built_in of string * Type.t | User_defined of label * tag

  type state =
    { safe: bool
    ; analysis: analysis
    ; strings: label String_map.t
    ; vars:
        (Level.handle * Type.t * source_span option * knowledge) String_map.t
    ; types: Type.t String_map.t
    ; loop_stack: label list
    ; level: Level.t
    ; fundefs: fundef String_map.t
    ; frame_namer: Frame_namer.t
    ; frames: (Level.t * value ir option ref * Type.t) Label_map.t }

  module R = State (struct type t = state end)

  type data =
    | Value of value ir * Type.t * knowledge
    | Effect of effect ir
    | Branch of (label -> label -> effect ir)

  type ufn =
    { ufn_state: state
    ; ufn_params: (string * Type.t) list
    ; ufn_ty: Type.t
    ; ufn_body: expr t
    ; ufn_tag: tag }

  and _ t =
    | Expr : data R.t -> expr t
    | Target : data R.t -> target t
    | Decl : data R.t -> decl t
    | Typ : unit R.t -> typ t
    | Param : string * tag -> param t
    | Fn : (string * ufn * Level.t) R.t -> fn t

  open R
  open R.Syntax

  let ( ++ ) = I.seq

  let as_value = function
    | Value (v, ty, _) when Type.direct ty = `Unit ->
        (I.(perform (discard v) (const 0L)), ty, Unknown)
    | Value (v, ty, w) -> (v, ty, w)
    | Effect e -> (I.(perform e (const 0L)), Type.unit, Unknown)
    | Branch f ->
        (* The result is [1] if the positive branch is chosen. Otherwise, the
           result is [0]. *)
        let b = box () and pos = local_label () and neg = local_label () in
        ( I.(
            perform
              ( move (box b) (const 1L)
              ++ f pos neg ++ def neg
              ++ move (box b) (const 0L)
              ++ def pos )
              (box b))
        , Type.int
        , Unknown )

  let as_effect = function
    | Value (v, _, _) -> I.discard v
    | Effect e -> e
    | Branch f ->
        let l = local_label () in
        I.(f l l ++ def l)

  let as_branch = function
    | Value (v, _, _) ->
        (* Jump to the positive branch if the value is non-zero. *)
        fun pos neg -> I.(cjump `Not_equal v (const 0L) pos neg)
    | Effect e ->
        (* Perform the effect and unconditionally jump to the positive branch. *)
        fun pos _ -> e ++ I.(jump pos)
    | Branch f -> f

  let int x _ = Expr (pure (Value (I.(const x), Type.int, Known_int x)))
  let string_index = ref 0

  let string x _ =
    Expr
      (let* s = get in
       let* l =
         match String_map.find_opt x s.strings with
         | Some l -> pure l
         | None ->
             let i = !string_index in
             incr string_index ;
             let l = global_label ("str" ^ string_of_int i) in
             let strings = String_map.add x l s.strings in
             let* () = set {s with strings} in
             pure l
       in
       pure (Value (I.loc l, Type.string, Unknown)))

  let nil _ = Expr (pure (Value (I.(const 0L), Type.nil, Known_nil)))
  let k_dummy = Source_span.v (0, 0) (0, 1)

  let neg (Expr expr) _ =
    Expr
      (let* v, _, w = map as_value expr in
       pure
         (Value
            ( I.(sub (const 0L) v)
            , Type.int
            , match w with
              | Unknown -> Unknown
              | Known_int x -> Known_int (Int64.neg x)
              | _ -> assert false ) ))

  let cmp rel (Expr expr_lhs) (Expr expr_rhs) _ =
    Expr
      (let* v_lhs, ty, _ = map as_value expr_lhs in
       let* v_rhs, _, _ = map as_value expr_rhs in
       pure
         ( match Type.direct ty with
         | `String ->
             let c = F.call_external "fang_string_compare" [v_lhs; v_rhs] in
             Branch I.(cjump rel c (const 0L))
         | _ -> Branch (I.cjump rel v_lhs v_rhs) ))

  let value (Target target) _ = Expr target

  let if_ ~condition:(Expr expr_condition) ~yes:(Expr expr_yes) ?no _ =
    Expr
      (let* branch = map as_branch expr_condition in
       let pos = local_label () and neg = local_label () in
       match no with
       | None ->
           let* v_yes, _, _ = map as_value expr_yes in
           pure
             (Effect I.(branch pos neg ++ def pos ++ discard v_yes ++ def neg))
       | Some (Expr expr_no) ->
           let* v_yes, ty, w_yes = map as_value expr_yes in
           let* v_no, _, w_no = map as_value expr_no in
           let b = box () and join = local_label () in
           pure
             (Value
                ( I.(
                    perform
                      ( branch pos neg ++ def pos
                      ++ move (box b) v_yes
                      ++ jump join ++ def neg
                      ++ move (box b) v_no
                      ++ def join )
                      (box b))
                , ty
                , join_knowledge w_yes w_no ) ))

  let knowledge_arith f w1 w2 =
    match (w1, w2) with
    | Unknown, _ | _, Unknown -> Unknown
    | Known_int x, Known_int y -> Known_int (f x y)
    | _ -> assert false

  let arith oper lhs rhs _ =
    match oper with
    | `And -> if_ ~condition:lhs ~yes:rhs ~no:(int 0L k_dummy) k_dummy
    | `Or -> if_ ~condition:lhs ~yes:(int 1L k_dummy) ~no:rhs k_dummy
    | (`Add | `Subtract | `Multiply | `Divide) as oper ->
        Expr
          (let (Expr expr_lhs) = lhs and (Expr expr_rhs) = rhs in
           let* v_lhs, _, w_lhs = map as_value expr_lhs in
           let* v_rhs, _, w_rhs = map as_value expr_rhs in
           let v, w =
             match oper with
             | `Add -> (I.add v_lhs v_rhs, knowledge_arith Int64.add w_lhs w_rhs)
             | `Subtract ->
                 (I.sub v_lhs v_rhs, knowledge_arith Int64.sub w_lhs w_rhs)
             | `Multiply ->
                 (I.mul v_lhs v_rhs, knowledge_arith Int64.mul w_lhs w_rhs)
             | `Divide -> (
                 ( I.div v_lhs v_rhs
                 , match w_rhs with
                   | Known_int 0L -> Unknown
                   | _ -> knowledge_arith Int64.div w_lhs w_rhs ) ) in
           pure (Value (v, Type.int, w)) )

  let while_ ~condition:(Expr expr_condition) ~body:(Expr expr_body) _ =
    Expr
      (let* branch = map as_branch expr_condition in
       let loop_begin = local_label ()
       and loop_body = local_label ()
       and loop_end = local_label () in
       let* s = get in
       let loop_stack = loop_end :: s.loop_stack in
       let* () = set {s with loop_stack} in
       let* v_body, _, _ = map as_value expr_body in
       let* s = get in
       let* () = set {s with loop_stack= List.tl s.loop_stack} in
       pure
         (Effect
            I.(
              def loop_begin ++ branch loop_body loop_end ++ def loop_body
              ++ discard v_body ++ jump loop_begin ++ def loop_end) ))

  let break _ =
    Expr
      (let* s = get in
       let l = List.hd s.loop_stack in
       pure (Effect I.(jump l)))

  let seq exprs _ =
    Expr
      ( match exprs with
      | [] -> pure (Value (I.const 0L, Type.unit, Unknown))
      | [Expr expr] -> expr
      | Expr expr :: exprs ->
          let rec loop er = function
            | [] -> assert false
            | [Expr expr] ->
                let* v, ty, w = map as_value expr in
                pure (Value (I.perform er v, ty, w))
            | Expr expr :: exprs ->
                let* e = map as_effect expr in
                loop (er ++ e) exprs in
          let* e = map as_effect expr in
          loop e exprs )

  let static_link_for_callee defn_level =
    let* s = get in
    match Level.depth defn_level - Level.depth s.level with
    | 0 -> pure (Level.static_link ~address:F.address s.level)
    | 1 -> pure F.address
    | n when n < 0 ->
        let rec loop count address level =
          if count = 0 then Level.static_link ~address level
          else
            let address = Level.static_link ~address level in
            loop (count - 1) address (Level.parent level) in
        pure (loop (-n) F.address s.level)
    | _ -> assert false

  let call name args _ =
    Expr
      (let* vs =
         traverse
           (fun (Expr expr) ->
             let+ v, _, _ = map as_value expr in
             v )
           args
       in
       let* s = get in
       match String_map.find name s.fundefs with
       | Built_in (external_name, ty) ->
           pure (Value (F.call_external external_name vs, ty, Unknown))
       | User_defined (l, _k) ->
           let level, _, ty = Label_map.find l s.frames in
           let* link = static_link_for_callee level in
           pure (Value (I.(call (loc l) (link :: vs)), ty, Unknown)))

  let uniform_array name ~size:(Expr expr_size) ~initial:(Expr expr_initial) _ =
    Expr
      (let* size, _, w = map as_value expr_size in
       let* initial, _, _ = map as_value expr_initial in
       let* s = get in
       let ty = String_map.find name s.types in
       pure
         (Value
            ( F.call_external "fang_alloc_array" [size; initial]
            , ty
            , match w with Known_int x -> Known_size x | _ -> Unknown ) ))

  let array name exprs k =
    Expr
      (let size = Int64.of_int (List.length exprs) in
       let (Expr arr) =
         uniform_array name ~size:(int size k) ~initial:(nil k) k in
       let* base, ty, w = map as_value arr in
       let b = box () in
       let count = ref 0 in
       let* assignments =
         traverse
           (fun (Expr expr) ->
             let* v, _, _ = map as_value expr in
             let i = !count in
             let address =
               I.(
                 add (box b)
                   (mul
                      (const (Int64.of_int F.word_size))
                      (add (const (Int64.of_int i)) (const 1L)) )) in
             incr count ;
             pure I.(move (mem address) v) )
           exprs
       in
       let rec loop er = function
         | [] -> I.(perform er (box b))
         | assignment :: assignments -> loop (er ++ assignment) assignments
       in
       let v = loop I.(move (box b) base) assignments in
       pure (Value (v, ty, w)) )

  let size (Expr expr) _ =
    Expr
      (let* base, _, w_arg = map as_value expr in
       match w_arg with
       | Known_size x -> pure (Value (I.const x, Type.int, Known_int x))
       | _ -> pure (Value (I.mem base, Type.int, Unknown)))

  let record name bindings _ =
    Expr
      (let* s = get in
       let ty = String_map.find name s.types in
       let size = I.const (Int64.of_int (List.length bindings)) in
       let base = F.call_external "fang_alloc_record" [size] in
       let sorted_bindings =
         List.sort
           (fun (name1, _) (name2, _) -> String.compare name1 name2)
           bindings in
       match sorted_bindings with
       | [] -> pure (Value (base, ty, Known_non_nil))
       | sorted_bindings ->
           let b = box () in
           let e = I.(move (box b) base) in
           let rec loop er i = function
             | [] -> pure (Value (I.(perform er (box b)), ty, Known_non_nil))
             | (_, Expr expr) :: sorted_bindings ->
                 let* v, _, _ = map as_value expr in
                 let address =
                   I.(
                     add (box b)
                       (mul (const (Int64.of_int F.word_size)) (const i))) in
                 let e = I.(move (mem address) v) in
                 loop (er ++ e) (Int64.succ i) sorted_bindings in
           loop e Int64.zero sorted_bindings)

  let scope decls (Expr expr) _ =
    Expr
      (let* s0 = get in
       let restore =
         let* s = get in
         set {s with vars= s0.vars; types= s0.types; fundefs= s0.fundefs} in
       match decls with
       | [] -> expr
       | [Decl decl] ->
           let* e = map as_effect decl in
           let* v, ty, w = map as_value expr in
           let* () = restore in
           pure (Value (I.perform e v, ty, w))
       | Decl decl :: decls ->
           let* e0 = map as_effect decl in
           let* es = traverse (fun (Decl decl) -> map as_effect decl) decls in
           let er = List.fold_left ( ++ ) e0 es in
           let* v, ty, w = map as_value expr in
           let* () = restore in
           pure (Value (I.perform er v, ty, w)))

  let assign (Target target) (Expr expr) _ =
    Expr
      (let* v_dst, _, _ = map as_value target in
       let* v_src, _, _ = map as_value expr in
       pure (Effect I.(move v_dst v_src)))

  let name name _ =
    Target
      (let* s = get in
       let h, ty, k_var, w = String_map.find name s.vars in
       let v = Level.access ~current:s.level h in
       pure
         (Value
            ( v
            , ty
            , match k_var with
              | None -> Unknown
              | Some k_var -> (
                match Property.query k_var (mutability s.analysis) with
                | `Constant -> w
                | `Mutates -> Unknown ) ) ))

  let index (Target target) (Expr expr) _ =
    Target
      (let* base, ty, w_target = map as_value target in
       let* index, _, w_expr = map as_value expr in
       let ty_item =
         match Type.direct ty with
         | `Array ty_item -> ty_item
         | _ -> assert false in
       let* s = get in
       let unsafe_value =
         let address =
           I.(
             add base
               (mul (const (Int64.of_int F.word_size)) (add index (const 1L))))
         in
         pure (Value (I.(mem address), ty_item, Unknown)) in
       match (s.safe, (w_target, w_expr)) with
       | _, (Known_size known_size, Known_int known_index)
         when known_index < 0L || known_index >= known_size ->
           (* This call will always fail at runtime. *)
           pure
             (Value
                ( I.mem (F.call_external "fang_check_array_index" [base; index])
                , ty_item
                , Unknown ) )
       | _, (Known_size known_size, Known_int known_index)
         when known_index >= 0L && known_index < known_size ->
           unsafe_value
       | false, _ -> unsafe_value
       | true, _ ->
           let b_base = box () and b_index = box () in
           let address =
             I.(
               add (box b_base)
                 (mul
                    (const (Int64.of_int F.word_size))
                    (add (box b_index) (const 1L)) )) in
           pure
             (Value
                ( I.(
                    perform
                      ( move (box b_base) base
                      ++ move (box b_index) index
                      ++ discard
                           (F.call_external "fang_check_array_index"
                              [box b_base; box b_index] ) )
                      (mem address))
                , ty_item
                , Unknown ) ))

  let assoc_with_index eq target pairs =
    let rec loop i = function
      | [] -> assert false
      | (key, value) :: pairs ->
          if eq key target then (value, i) else loop (i + 1) pairs in
    loop 0 pairs

  let access (Target target) name _ =
    Target
      (let* base, ty, w = map as_value target in
       let b = box () in
       let ty_field, index =
         match Type.direct ty with
         | `Record fields -> assoc_with_index String.equal name fields
         | _ -> assert false in
       let address =
         I.(
           add (box b)
             (mul
                (const (Int64.of_int F.word_size))
                (const (Int64.of_int index)) )) in
       let* s = get in
       match (s.safe, w) with
       | _, Known_nil ->
           pure
             (Value
                ( I.mem (F.call_external "fang_error_nil_access" [])
                , ty_field
                , Unknown ) )
       | false, _ | true, Known_non_nil ->
           pure
             (Value
                ( I.(perform (move (box b) base) (mem address))
                , ty_field
                , Unknown ) )
       | true, _ ->
           let good = local_label () and bad = local_label () in
           pure
             (Value
                ( I.(
                    perform
                      ( move (box b) base
                      ++ cjump `Equal (box b) (const 0L) bad good
                      ++ def bad
                      ++ discard (F.call_external "fang_error_nil_access" [])
                      ++ def good )
                      (mem address))
                , ty_field
                , Unknown ) ))

  let var name ?type_name (Expr expr) k =
    Decl
      (let* v, ty, w = map as_value expr in
       let* s = get in
       let ty =
         match type_name with
         | None -> ty
         | Some type_name -> String_map.find type_name s.types in
       let scope = Property.query k (escaping s.analysis) in
       let h, level = Level.alloc scope s.level in
       let dst = Level.access ~current:level h in
       let* () =
         set {s with level; vars= String_map.add name (h, ty, Some k, w) s.vars}
       in
       pure (Effect I.(move dst v)))

  let for_ var_name ~initial ~final:(Expr expr_final) ~body:(Expr expr_body) k =
    Expr
      (let* s0 = get in
       let (Decl decl_var) = var var_name initial k in
       let* e = map as_effect decl_var in
       let (Expr expr_initial) = value (name var_name k) k in
       let* v_initial, _, _ = map as_value expr_initial in
       (* We'd like to be able to perform a literal comparison when the
          upper-bound is statically known, but we'll always store the
          upper-bound in a register for flexibility during code-generation (we
          can't compare a register to a 64 bit literal with x86-64). *)
       let* v_final, _, _ = map as_value expr_final in
       let v_final, e_prelude =
         let b = box () in
         (I.box b, e ++ I.(move (box b) v_final)) in
       let* e_body = map as_effect expr_body in
       let loop_body = local_label ()
       and loop_increment = local_label ()
       and loop_end = local_label () in
       let* s = get in
       let* () = set {s with vars= s0.vars} in
       pure
         (Effect
            (* This more complicated loop structure ensures that no overflow
               occurs when the upper-bound is the largest 64 bit integer. *)
            I.(
              e_prelude
              ++ cjump `Less_or_equal v_initial v_final loop_body loop_end
              ++ def loop_body ++ e_body
              ++ cjump `Less v_initial v_final loop_increment loop_end
              ++ def loop_increment
              ++ move v_initial (add v_initial (const 1L))
              ++ jump loop_body ++ def loop_end) ))

  module Merge (M : Map.S) = struct
    let apply m1 m2 =
      M.merge
        (fun _ x1 x2 ->
          match (x1, x2) with
          | None, _ -> x2
          | _, Some _ -> x2
          | Some _, None -> x1 )
        m1 m2
  end

  module Merge_string_map = Merge (String_map)
  module Merge_label_map = Merge (Label_map)

  let evaluate_fn_body (group_fundefs, group_frames) ufn level =
    (* Restored after evaluating the body. *)
    let* s0 = get in
    (* Add a [var] for each parameter. *)
    let i = ref 0 in
    let vars =
      List.fold_left
        (fun vars (param_name, ty) ->
          let h = Level.args level !i in
          incr i ;
          String_map.add param_name (h, ty, None, Unknown) vars )
        ufn.ufn_state.vars ufn.ufn_params in
    (* Augment the existing frames with the ones from this group of function
       definitions. *)
    let frames = Merge_label_map.apply ufn.ufn_state.frames group_frames in
    (* Augment the existing fundefs with the ones from this group of function
       definitions. *)
    let fundefs = Merge_string_map.apply ufn.ufn_state.fundefs group_fundefs in
    (* Set the new state and evaluate the body. *)
    let* () = set {ufn.ufn_state with vars; level; frames; fundefs} in
    let (Expr expr_body) = ufn.ufn_body in
    let* v, _, _ = map as_value expr_body in
    (* Preserve any new strings or frames that resulted from evaluating the
       body. *)
    let* s = get in
    let frames =
      Merge_label_map.apply s0.frames
        (Label_map.update (Level.label level)
           (function
             | Some (_, vref, ufn) ->
                 vref := Some v ;
                 Some (s.level, vref, ufn)
             | None -> assert false )
           s.frames ) in
    let strings = Merge_string_map.apply s0.strings s.strings in
    set {s0 with frames; strings}

  let fns fs _ =
    Decl
      (let* triples = traverse (fun (Fn fn) -> fn) fs in
       let group_fundefs =
         List.fold_right
           (fun (name, ufn, level) fundefs ->
             String_map.add name
               (User_defined (Level.label level, ufn.ufn_tag))
               fundefs )
           triples String_map.empty in
       let group_frames =
         List.fold_right
           (fun (_, ufn, level) self_frames ->
             Label_map.add (Level.label level)
               (level, ref None, ufn.ufn_ty)
               self_frames )
           triples Label_map.empty in
       let* () =
         let rec loop = function
           | [] ->
               let* s = get in
               let fundefs = Merge_string_map.apply s.fundefs group_fundefs in
               set {s with fundefs}
           | (_, ufn, level) :: triples ->
               let* () =
                 evaluate_fn_body (group_fundefs, group_frames) ufn level
               in
               loop triples in
         loop triples
       in
       pure (Effect I.(discard (const 0L))))

  let types ts _ =
    Decl
      (let* () = traverse_unit (fun (Typ typ) -> typ) ts in
       let* s = get in
       let assoc name = String_map.find name s.types in
       String_map.iter (fun _ ty -> Type.resolve assoc ty) s.types ;
       pure (Effect I.(discard (const 0L))))

  let fn name params ?type_name body k =
    Fn
      (let* s = get in
       let triples =
         List.map
           (fun (Param (param_name, k), type_name) ->
             (param_name, k, String_map.find type_name s.types) )
           params in
       let ty =
         match type_name with
         | None -> Type.unit
         | Some type_name -> String_map.find type_name s.types in
       let scopes =
         List.map
           (fun k -> Property.query k (escaping s.analysis))
           (List.map (fun (_, k, _) -> k) triples) in
       let reach = Property.query k (leaves s.analysis) in
       let level = Level.make ~parent:s.level s.frame_namer name scopes reach in
       let ufn =
         { ufn_state= s
         ; ufn_params= List.map (fun (name, _, ty) -> (name, ty)) triples
         ; ufn_ty= ty
         ; ufn_body= body
         ; ufn_tag= k } in
       pure (name, ufn, level))

  let param name k = Param (name, k)

  let alias_type ~name ~target _ =
    Typ
      (let* s = get in
       let types =
         String_map.add name (Type.alias (Type.named target)) s.types in
       set {s with types})

  let array_type ~name ~item _ =
    Typ
      (let* s = get in
       let types = String_map.add name (Type.array (Type.named item)) s.types in
       set {s with types})

  let record_type name fields _ =
    Typ
      (let* s = get in
       let sorted_fields =
         List.sort
           (fun (name1, _) (name2, _) -> String.compare name1 name2)
           fields in
       let ty =
         Type.record
           (List.map
              (fun (name, type_name) -> (name, Type.named type_name))
              sorted_fields ) in
       let types = String_map.add name ty s.types in
       set {s with types})

  let translate ?(safe = true) analysis (Expr expr) =
    let s0 =
      { safe
      ; analysis
      ; strings= String_map.empty
      ; vars= String_map.empty
      ; types=
          String_map.(empty |> add "int" Type.int |> add "string" Type.string)
      ; loop_stack= []
      ; level= Level.outermost
      ; fundefs=
          String_map.(
            empty
            |> add "chr" (Built_in ("fang_string_char", Type.int))
            |> add "concat" (Built_in ("fang_string_concat", Type.string))
            |> add "exit" (Built_in ("fang_io_exit", Type.unit))
            |> add "flush" (Built_in ("fang_io_flush", Type.unit))
            |> add "read_char" (Built_in ("fang_io_read_char", Type.string))
            |> add "not" (Built_in ("fang_not", Type.int))
            |> add "ord" (Built_in ("fang_string_ord", Type.int))
            |> add "print" (Built_in ("fang_io_print", Type.unit))
            |> add "print_int" (Built_in ("fang_io_print_int", Type.unit))
            |> add "print_line" (Built_in ("fang_io_print_line", Type.unit))
            |> add "len" (Built_in ("fang_string_length", Type.int))
            |> add "substring" (Built_in ("fang_string_sub", Type.string))
            |> add "random" (Built_in ("fang_io_random", Type.int))
            |> add "seed" (Built_in ("fang_io_seed_random", Type.unit))
            |> add "error" (Built_in ("fang_io_tiger_error", Type.unit)))
      ; frame_namer= Frame_namer.create ()
      ; frames= Label_map.empty } in
    let (v, _, _), s = (map as_value expr) s0 in
    let string_fragments =
      Iter.adapt2 String_map.iter s.strings |> Iter.map (fun (s, l) -> (l, s))
    in
    let outermost_frame, outermost_frame_ptr =
      F.finalize v (Level.frame s.level) in
    (* Even if the outermost frame requires no stack space, it still might need
       a frame pointer so that it can be provided as the static link to a
       function. For simplicity, we create a frame pointer if there are any
       functions defined (even if they're never invoked). *)
    let outermost_frame_ptr =
      if
        outermost_frame_ptr = `No_frame_ptr && not (Label_map.is_empty s.frames)
      then `Frame_ptr 0
      else outermost_frame_ptr in
    let frame_fragments =
      Iter.cons
        (Level.label Level.outermost, (outermost_frame, outermost_frame_ptr))
        ( Iter.adapt2 Label_map.iter s.frames
        |> Iter.map (fun (l, (level, vref, _)) ->
               let v = F.finalize (Option.get !vref) (Level.frame level) in
               (l, v) ) ) in
    (frame_fragments, string_fragments)
end

let pp_label = Label.pp ()

let pp_string_fragment ppf (l, s) =
  Fmt.pf ppf "@[<2>(@[<h>string@ %a@]@ %S)@]" pp_label l s

let pp_fp ppf = function
  | `No_frame_ptr -> Fmt.string ppf "no-fp"
  | `Frame_ptr stack_size -> Fmt.pf ppf "@[(fp@ %d)@]" stack_size

let pp_frame_fragment pp_box ppf (l, (v, fp)) =
  Fmt.pf ppf "@[<2>(@[<h>frame@ %a@ %a@]@ %a)@]" pp_label l pp_fp fp
    (Fang_ir.Pretty.pp pp_box) v
