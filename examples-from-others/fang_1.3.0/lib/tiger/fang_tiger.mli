(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

(** The Tiger language.

    The language is encoded in the
    {{:http://okmij.org/ftp/tagless-final/index.html} tagless-final} style. *)

open Fang_iter

(** {1 Source file locations} *)

(** A span in a Tiger source file.

    Spans are defined by a start point (the "origin") and an end point (the
    "terminus").

    The origin and terminus must define a contiguous span in the file. If they
    don't, the span is invalid. *)
type source_span

module Source_span : sig
  type t = source_span

  (** A location in a Tiger source file, defined by row and column indicies.

      Both indicies are one-based. *)
  type point = int * int

  exception Invalid of point * point

  val v : point -> point -> t
  (** Define a span bounded by the two points.

      Raises {!Invalid} if the pair of points don't form a valid span. *)

  val from_lexing_positions : Lexing.position -> Lexing.position -> t
  val compare : t -> t -> int
  val compare_point : point -> point -> int
  val equal : t -> t -> bool
  val pp : t Fmt.t [@@ocaml.toplevel_printer]
  val origin : t -> point
  val terminus : t -> point
  val line : point -> int
  val column : point -> int

  module Map : Map.S with type key = t
end

(** {1 Language specification} *)

type expr = private Expr_tag
type target = private Target_tag
type decl = private Decl_tag
type fn = private Fn_tag
type param = private Param_tag
type typ = private Typ_tag
type oper = [`Add | `Subtract | `Multiply | `Divide | `And | `Or]

type rel =
  [`Equal | `Not_equal | `Less | `Less_or_equal | `Greater | `Greater_or_equal]

type tag = source_span

(** The Tiger language.

    Every Tiger fragment has a {!tag} which stores its location in a source
    file. In the examples that follow, the tag argument is omitted for clarity. *)
module type TIGER = sig
  (** A Tiger fragment. *)
  type _ t

  (** {2 Expressions} *)

  val int : int64 -> tag -> expr t
  (** A 64 bit integer literal. *)

  val string : string -> tag -> expr t
  (** A string literal. *)

  val nil : tag -> expr t
  (** The empty record value. *)

  val arith : oper -> expr t -> expr t -> tag -> expr t
  (** Arithmetic on [int].

      For example,

      {v 23 * (5 + 3) v}

      is

      {[ arith `Multiply (int 23L) (arith `Add (int 5L) (int 3L)) ]} *)

  val neg : expr t -> tag -> expr t
  (** Negate an [int].

      For example,

      {v -(3 + 5) v}

      is

      {[ neg (arith `Add (int 3L) (int 5L)) ]} *)

  val cmp : rel -> expr t -> expr t -> tag -> expr t
  (** Compare two values.

      For example,

      {v "foo" = "bar" v}

      is

      {[ cmp `Equal (string "foo") (string "bar") ]}

      Only values of compatible type can be compared.

      Allowed comparisons:

      - [int]: All
      - [string]: All (lexicographic)
      - arrays: equality (by reference)
      - records: equality (by reference) *)

  val value : target t -> tag -> expr t
  (** The value of a target.

      For example,

      {v x + 3 v}

      is

      {[ arith `Add (value (name "x")) (int 3L) ]} *)

  val if_ : condition:expr t -> yes:expr t -> ?no:expr t -> tag -> expr t
  (** Conditional values.

      There are two forms of [if] statements. In both cases, [condition] must be
      of type [int].

      The first is for conditionally performing an effect. In this case, the
      affirmative branch is of type [unit].

      For example,

      {v if a < 10 then print("yes") v}

      is

      {[
        if_
          ~condition:(cmp `Less_than (value (name "a")) (int 10L))
          ~yes:(call "print" [string "yes"])
      ]}

      When a negative branch is included ([no]), both branches must evaluate to
      values of compatible types.

      For example,

      {v if a < 10 then a + 1 else 100 v}

      is

      {[
        if_
          ~condition:(cmp `Less_than (value (name "a")) (int 10L))
          ~yes:(arith `Add (value (name "a")) (int 1L))
          ~no:(int 100L)
      ]} *)

  val while_ : condition:expr t -> body:expr t -> tag -> expr t
  (** Conditional repeated execution.

      The [condition] must be of type [int] and the [body] must be of type
      [unit].

      For example,

      {v while x >= 10 do x := x - 5 v}

      is

      {[
        while_
          ~condition:(cmp `Greater_or_equal (value (name "x")) (int 10L))
          ~body:
            (assign (name "x") (arith `Subtract (value (name "x")) (int 5L)))
      ]} *)

  val for_ :
    string -> initial:expr t -> final:expr t -> body:expr t -> tag -> expr t
  (** The [for] loop we know and love.

      [initial] and [final] must be of type [int]. The variable takes on the
      values from [intial, initial + 1, ..., value] inclusive and is not defined
      outside the body of the loop.

      The body must be of type [unit].

      For example,

      {v for i := 1 to 10 do print_int(i + 1) v}

      is

      {[
        for_ "i" ~initial:(int 1L) ~final:(int 10)
          ~body:(call "print_int" [arith `Add (value (name "i")) (int 1L)])
      ]} *)

  val break : tag -> expr t
  (** Break out of a loop.

      For example,

      {v while 1 do break v}

      is

      {[ while_ ~condition:(int 1L) ~body:break ]} *)

  val seq : expr t list -> tag -> expr t
  (** Sequence multiple expressions.

      The result is the last value in the sequence.

      For example,

      {v (print("foo"); 10; 20) v}

      is

      {[ seq [call "print" [string "foo"]; int 10L; int 20L] ]} *)

  val call : string -> expr t list -> tag -> expr t
  (** Invoke a function.

      For example,

      {v concat(s, "foo") v}

      is

      {[ call "concat" [value (name "s"); string "foo"] ]} *)

  val uniform_array : string -> size:expr t -> initial:expr t -> tag -> expr t
  (** A new array filled with a uniform value.

      [size] must be of type [int].

      For example,

      {v people["Sally"] of 100 v}

      (an array of 100 people named Sally) is

      {[ uniform_array "people" ~size:(int 100L) ~initial:(string "Sally") ]} *)

  val array : string -> expr t list -> tag -> expr t
  (** An array literal.

      For example,

      {v people of ["Joe", "Sally", "Bob"] v}

      is

      {[ array "people" [string "Joe"; string "Sally"; string "Bob"] ]} *)

  val size : expr t -> tag -> expr t
  (** The number of items in an array value.

      For example,

      {v size numbers v}

      is

      {[ size (value (name "numbers")) ]} *)

  val record : string -> (string * expr t) list -> tag -> expr t
  (** Create a new record value.

      The record fields can be specified in any order.

      For example,

      {v person {name="Joe", age=11} v}

      is

      {[ record "person" [("name", string "Joe"); ("age", int 11L)] ]} *)

  val scope : decl t list -> expr t -> tag -> expr t
  (** Define a new scope.

      The expression is evaluated based on the new declarations. After, the
      declarations no longer apply.

      For example,

      {v
let
  var x := 10
  var y := x + 1
in
  x - y
end
      v}

      is

      {[
        scope
          [var "x" (int 10L); var "y" (arith `Add (value (name "x")) (int 1L))]
          (arith `Subtract (value (name "x")) (value (name "y")))
      ]} *)

  val assign : target t -> expr t -> tag -> expr t
  (** Assign a value to a target.

      The type of the target must be compatible with the type of the value to be
      assigned.

      The result is a value of type [unit].

      For example,

      {v person.name := "Joe" v}

      is

      {[ assign (access (name "person") "name") (string "Joe") ]} *)

  (** {2 Targets} *)

  val name : string -> tag -> target t
  (** The name of a variable.

      For example,

      {v my_var v}

      is

      {[ name "my_var" ]} *)

  val index : target t -> expr t -> tag -> target t
  (** An indexed location in an array.

      The index value must of be type [int].

      For example,

      {v numbers[3] v}

      is

      {[ index (name "numbers") (int 3L) ]} *)

  val access : target t -> string -> tag -> target t
  (** A record field.

      For example,

      {v person.age v}

      is

      {[ access (name "person") "age" ]} *)

  (** {2 Declarations} *)

  val var : string -> ?type_name:string -> expr t -> tag -> decl t
  (** Variable declaration.

      In most cases, the type of a variable can be inferred from the value it is
      assigned.

      For example,

      {v var x := 10 v}

      is

      {[ var "x" (int 10L) ]}

      The type can be also supplied explicitly, like

      {v var y: int := 20 v}

      which is

      {[ var "y" ~type_name:"int" (int 20L) ]}

      When a variable is initialized to [nil], the type must be included:

      {v var p: person := nil v} *)

  val fns : fn t list -> tag -> decl t
  (** A mutually-recursive group of function definitions.

      For example,

      {v
function is_even(x: int): int =
  if x = 0 then 1
  else is_odd(x - 1)

and function is_odd(x: int): int =
  if x = 0 then 0
  else is_even(x - 1)
      v}

      is

      {[
        fns
          [ fn "is_even"
              [(param "x", "int")]
              ~type_name:"int"
              (if_
                 ~condition:(cmp `Equal (value (name "x")) (int 0L))
                 ~yes:(int 1L)
                 ~no:
                   (call "is_odd" [arith `Subtract (value (name "x")) (int 1L)]) )
          ; fn "is_odd"
              [(param "x", "int")]
              ~type_name:"int"
              (if_
                 ~condition:(cmp `Equal (value (name "x")) (int 0L))
                 ~yes:(int 0L)
                 ~no:
                   (call "is_even"
                      [arith `Subtract (value (name "x")) (int 1L)] ) ) ]
      ]} *)

  val types : typ t list -> tag -> decl t
  (** A mutually-recursive group of type definitions.

      For example,

      {v
type participants = array of person
and type person = string
      v}

      is

      {[
        types
          [ array_type ~name:"participants" ~item:"person"
          ; alias_type ~name:"person" ~target:"string" ]
      ]} *)

  (** {2 Functions} *)

  val fn :
       string
    -> (param t * string) list
    -> ?type_name:string
    -> expr t
    -> tag
    -> fn t
  (** Function definition.

      When [type_name] is not provided, the function body must be of type
      [unit].

      For example,

      {v function greet() = print("Hi!") v}

      is

      {[ fn "greet" [] (call "print" [string "Hi!"]) ]}

      and

      {v function add(x: int, y: int): int = x + y v}

      is

      {[
        fn "add"
          [(param "x", "int"); (param "y", "int")]
          ~type_name:"int"
          (arith `Add (value (name "x")) (value (name "y")))
      ]} *)

  (** {2 Function parameter names} *)

  val param : string -> tag -> param t
  (** A function parameter name.

      For example, in the function definition

      {v function add(x: int, y: int): int = x + y v}

      both [x] and [y] are parameters. *)

  (** {2 Types} *)

  val alias_type : name:string -> target:string -> tag -> typ t
  (** Define a type alias.

      For example,

      {v type age = int v}

      is

      {[ alias_type ~name:"age" ~target:"int" ]} *)

  val array_type : name:string -> item:string -> tag -> typ t
  (** Define an array type.

      For example,

      {v type numbers = array of int v}

      is

      {[ array_type ~name:"numbers" ~item:"int" ]} *)

  val record_type : string -> (string * string) list -> tag -> typ t
  (** Define a record type.

      For example,

      {v type person = {name: string, age: int} v}

      is

      {[ record_type "person" [("name", "string"); ("age", "int")] ]} *)
end

(** {1 Validation (type-checking)} *)

type validation_error = unit Fmt.t

(** Semantic analysis and type-checking of Tiger fragments. *)
module Validation (L : TIGER) : sig
  (** A validator for a Tiger fragment. *)
  type _ t

  val validate : expr t -> (expr L.t, validation_error * source_span) result

  (** {1 Building validators} *)

  include TIGER with type 'a t := 'a t
end

(** {1 Pretty-printing} *)

(** Pretty-printing Tiger fragments. *)
module Pretty : sig
  (** A pretty-printer for a Tiger fragment. *)
  type _ t

  val pp : _ t Fmt.t

  (** {1 Building pretty-printers} *)

  include TIGER with type 'a t := 'a t  (** @closed *)
end

(** {1 Parsing} *)

type parsing_error =
  [ `Syntax_error
  | `Invalid_string_character of char
  | `Unterminated_string
  | `Invalid_token of string ]

val pp_parsing_error : parsing_error Fmt.t

(** Parsing Tiger fragments. *)
module Parsing (L : TIGER) : sig
  val parse : Lexing.lexbuf -> (expr L.t, parsing_error * source_span) result
  (** Parse a Tiger expression.

      Note that the {!Lexing.lexbuf} {b must} have position information, since
      the parser produces source spans for each fragment. *)
end

(** {1 Static analysis} *)

(** Accumulated knowledge about a Tiger fragment based on static analysis. *)
type analysis

(** A specific property of interest about elements of Tiger fragments.

    Values of type ['a property] are mappings from tags to values of type ['a].

    If an [int property] is only applicable to variables, for example, the tag
    of every {!TIGER.var} will have a corresponding [int] value. *)
type _ property

val escaping : analysis -> [`Local | `Escapes] property
(** A variable or parameter is said to "escape" if it is referenced at a static
    function depth greater than the depth of its definition.

    A variable or parameter is "local" if it does not escape.

    Knowing whether variables and parameters escape or not is important for
    translation into the intermediate representation, because storage locations
    with local scope can fit in efficient registers instead of memory.

    For example, consider this Tiger fragment:

    {v
let
  var x := 10
  function add(y: int): int = x + y
in
  x + 1
end
    v}

    In this example:

    - [x] escapes because it is referenced in the body of [add].

    - [y] is local because it is not referenced outside the function where it's
      defined. *)

val mutability : analysis -> [`Constant | `Mutates] property
(** A variable assigned to after its initial definition is said to "mutate".

    This information is useful during translation to the intermediate
    representation because it determines whether statically-inferred facts about
    variables apply.

    For example, consider this Tiger snippet:

    {v
let
  type numbers = array of int
  var xs := numbers[5] of 10
  var ys := xs
in
  ys[3] := 100
end
    v}

    The size of [xs] is statically known to be 5. Since [ys] is initialized to
    [xs] and both [xs] and [ys] are constant, we know the size of [ys] is also
    5.

    Knowing this, bounds-checks for the assignment to [ys\[3\]] can be elided. *)

val leaves : analysis -> [`Branch | `Leaf] property
(** Functions which don't call other user-defined Tiger functions are called
    "leaves".

    Knowing which functions are leaves is useful for translation into the
    intermediate representation because leaf functions don't need to store their
    static link in memory. *)

module Analysis (L : TIGER) : sig
  (** An analyzer for a Tiger fragment. *)
  type _ t

  val analyze : 'a t -> analysis * 'a L.t

  (** {1 Building analyzers} *)

  include TIGER with type 'a t := 'a t  (** @closed *)
end

module Property : sig
  type 'a t = 'a property

  exception Unknown of source_span

  val query : tag -> 'a t -> 'a
  (** Look up the value of a property for a particular element, by its tag.

      Raises {!Unknown} if this tag does have a corresponding property value. *)

  val iter : 'a t -> (tag * 'a) iter
end

(** {1 No-op interpreter} *)

(** The no-op interpreter, which can be useful for testing. *)
module Nop : TIGER with type _ t = unit
