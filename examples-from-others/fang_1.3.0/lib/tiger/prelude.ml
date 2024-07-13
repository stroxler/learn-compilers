(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

module Source_span : sig
  type point = int * int
  type t

  exception Invalid of point * point

  val v : point -> point -> t
  val from_lexing_positions : Lexing.position -> Lexing.position -> t
  val pp : t Fmt.t [@@ocaml.toplevel_printer]
  val compare : t -> t -> int
  val compare_point : point -> point -> int
  val equal : t -> t -> bool
  val origin : t -> point
  val terminus : t -> point
  val line : point -> int
  val column : point -> int

  module Map : Map.S with type key = t
end = struct
  type point = int * int
  type t = point * point

  exception Invalid of point * point

  let pp = Fmt.(box text_loc)
  let origin = fst
  let terminus = snd
  let line = fst
  let column = snd

  let v o t =
    if line o > line t || (line o = line t && column o > column t) then
      raise (Invalid (o, t))
    else (o, t)

  let from_lexing_positions start_p curr_p =
    let point_from_lexing_position pos =
      let line = pos.Lexing.pos_lnum and column = pos.pos_cnum - pos.pos_bol in
      (line, column) in
    (point_from_lexing_position start_p, point_from_lexing_position curr_p)

  let compare_point p1 p2 =
    let c = Int.compare (line p1) (line p2) in
    if c = 0 then Int.compare (column p1) (column p2) else c

  let compare t1 t2 =
    let c = compare_point (origin t1) (origin t2) in
    if c = 0 then compare_point (terminus t1) (terminus t2) else c

  let equal t1 t2 = compare t1 t2 = 0

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

let%test_module "source_span" =
  ( module struct
    let%test "[Invalid spans cannot be defined]" =
      try
        let _ = Source_span.v (1, 10) (1, 8) in
        false
      with Source_span.Invalid _ -> true

    let%expect_test "[Spans can be pretty-printed]" =
      Fmt.pr "%a@." Source_span.pp (Source_span.v (1, 1) (10, 5)) ;
      [%expect {| 1.1-10.5 |}]
  end )

type expr = Expr_tag
type target = Target_tag
type decl = Decl_tag
type fn = Fn_tag
type param = Param_tag
type typ = Typ_tag
type oper = [`Add | `Subtract | `Multiply | `Divide | `And | `Or]

type rel =
  [`Equal | `Not_equal | `Less | `Less_or_equal | `Greater | `Greater_or_equal]

type tag = Source_span.t

module type TIGER = sig
  type _ t

  val int : int64 -> tag -> expr t
  val string : string -> tag -> expr t
  val nil : tag -> expr t
  val arith : oper -> expr t -> expr t -> tag -> expr t
  val neg : expr t -> tag -> expr t
  val cmp : rel -> expr t -> expr t -> tag -> expr t
  val value : target t -> tag -> expr t
  val if_ : condition:expr t -> yes:expr t -> ?no:expr t -> tag -> expr t
  val while_ : condition:expr t -> body:expr t -> tag -> expr t

  val for_ :
    string -> initial:expr t -> final:expr t -> body:expr t -> tag -> expr t

  val break : tag -> expr t
  val seq : expr t list -> tag -> expr t
  val call : string -> expr t list -> tag -> expr t
  val uniform_array : string -> size:expr t -> initial:expr t -> tag -> expr t
  val array : string -> expr t list -> tag -> expr t
  val size : expr t -> tag -> expr t
  val record : string -> (string * expr t) list -> tag -> expr t
  val scope : decl t list -> expr t -> tag -> expr t
  val assign : target t -> expr t -> tag -> expr t
  val name : string -> tag -> target t
  val index : target t -> expr t -> tag -> target t
  val access : target t -> string -> tag -> target t
  val var : string -> ?type_name:string -> expr t -> tag -> decl t
  val fns : fn t list -> tag -> decl t
  val types : typ t list -> tag -> decl t

  val fn :
       string
    -> (param t * string) list
    -> ?type_name:string
    -> expr t
    -> tag
    -> fn t

  val param : string -> tag -> param t
  val alias_type : name:string -> target:string -> tag -> typ t
  val array_type : name:string -> item:string -> tag -> typ t
  val record_type : string -> (string * string) list -> tag -> typ t
end
