(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

%parameter <L : Prelude.TIGER>

%{
let source (start_p, curr_p) =
  Prelude.Source_span.from_lexing_positions start_p curr_p
%}

%start <Prelude.expr L.t> tiger
%%

tiger:
  | v=expr; EOF { v }
;

expr:
  | x=INT
    {
      L.int x (source $loc)
    }
  | x=STRING
    {
      L.string x (source $loc)
    }
  | NIL
    {
      L.nil (source $loc)
    }
  | BREAK
    {
      L.break (source $loc)
    }
  | name=IDENTIFIER; BRACKET_LEFT; size=expr; BRACKET_RIGHT; OF; initial=expr
    {
      L.uniform_array ~size ~initial name (source $loc)
    }
  | name=IDENTIFIER; OF; BRACKET_LEFT; vs=separated_list(COMMA, expr); BRACKET_RIGHT
    {
      L.array name vs (source $loc)
    }
  | name=IDENTIFIER; BRACE_LEFT; bindings=separated_list(COMMA, binding); BRACE_RIGHT
    {
      L.record name bindings (source $loc)
    }
  | name=IDENTIFIER
    {
      let where = source $loc in
      L.value (L.name name where) where
    }
  | name=IDENTIFIER; ASSIGN; e=expr
    {
      let target = L.name name (source $loc(name)) in
      L.assign target e (source $loc)
    }
  | SIZE; e=expr
    {
      L.size e (source $loc)
    }
  | t=target
    {
      L.value t (source $loc)
    }
  | t=target; ASSIGN; e=expr
    {
      L.assign t e (source $loc)
    }
  | lhs=expr; r=rel; rhs=expr
    {
      L.cmp r lhs rhs (source $loc)
    }
  | MINUS; e=expr %prec NEGATE
    {
      L.neg e (source $loc)
    }
  | lhs=expr; p=oper; rhs=expr
    {
      L.arith p lhs rhs (source $loc)
    }
  | sym=IDENTIFIER; PAREN_LEFT; es=separated_list(COMMA, expr); PAREN_RIGHT
    {
      L.call sym es (source $loc)
    }
  | PAREN_LEFT; es=separated_list(SEMI, expr); PAREN_RIGHT
    {
      L.seq es (source $loc)
    }
  | WHILE; condition=expr; DO; body=expr
    {
      L.while_ ~condition ~body (source $loc)
    }
  | FOR; name=IDENTIFIER; ASSIGN; initial=expr; TO; final=expr; DO; body=expr
    {
      L.for_ name ~initial ~final ~body (source $loc)
    }
  | IF; condition=expr; THEN; yes=expr
    {
      L.if_ ~condition ~yes (source $loc)
    }
  | IF; condition=expr; THEN; yes=expr; ELSE; no=expr
    {
      L.if_ ~condition ~yes ~no (source $loc)
    }
  | LET; ds=decl*; IN; vs=separated_list(SEMI, expr); END
    {
      L.scope ds L.(seq vs (source $loc(vs))) (source $loc)
    }
;

%inline
rel:
  | EQUAL { `Equal }
  | NOT_EQUAL { `Not_equal }
  | LESS { `Less }
  | LESS_EQUAL { `Less_or_equal }
  | GREATER { `Greater }
  | GREATER_EQUAL { `Greater_or_equal }
;

%inline
oper:
  | PLUS { `Add }
  | MINUS  { `Subtract }
  | STAR { `Multiply }
  | FORWARD_SLASH { `Divide }
  | LOGICAL_AND { `And }
  | LOGICAL_OR { `Or }
;

%inline
binding:
  | name=IDENTIFIER; EQUAL; e=expr
    {
      (name, e)
    }
;

target:
  | name=IDENTIFIER; BRACKET_LEFT; e=expr; BRACKET_RIGHT
    {
      let target = L.name name (source $loc(name)) in
      L.index target e (source $loc)
    }
  | q=target; BRACKET_LEFT; e=expr; BRACKET_RIGHT
    {
      L.index q e (source $loc)
    }
  | name=IDENTIFIER; DOT; field_name=IDENTIFIER
    {
      let target = L.name name (source $loc(name)) in
      L.access target field_name (source $loc)
    }
  | q=target; DOT; field_name=IDENTIFIER
    {
      L.access q field_name (source $loc)
    }
;

decl:
  | VAR; sym=IDENTIFIER; ASSIGN; e=expr
    {
      L.var sym e (source $loc)
    }
  | VAR; sym=IDENTIFIER; COLON; type_name=IDENTIFIER; ASSIGN; e=expr
    {
      L.var sym ~type_name e (source $loc)
    }
  | f=fn
    {
      L.fns [f] (source $loc)
    }
  | f=fn; AND; fs=separated_nonempty_list(AND, fn)
    {
      L.fns (f :: fs) (source $loc)
    }
  | y=typ
    {
      L.types [y] (source $loc)
    }
  | y=typ; AND; ys=separated_nonempty_list(AND, typ)
    {
      L.types (y :: ys) (source $loc)
    }
;

fn:
  | FUNCTION; name=IDENTIFIER; PAREN_LEFT; params=separated_list(COMMA, param); PAREN_RIGHT; EQUAL; body=expr
    {
      L.fn name params body (source $loc)
    }
  | FUNCTION; name=IDENTIFIER; PAREN_LEFT; params=separated_list(COMMA, param); PAREN_RIGHT; COLON; type_name=IDENTIFIER; EQUAL; body=expr
    {
      L.fn name params ~type_name body (source $loc)
    }
;

%inline
param:
  | name=IDENTIFIER; COLON; type_name=IDENTIFIER
    {
      let param = L.param name (source $loc(name)) in
      (param, type_name)
    }
;

typ:
  | TYPE; name=IDENTIFIER; EQUAL; target=IDENTIFIER
    {
      L.alias_type ~name ~target (source $loc)
    }
  | TYPE; name=IDENTIFIER; EQUAL; ARRAY; OF; item=IDENTIFIER
    {
      L.array_type ~name ~item (source $loc)
    }
  | TYPE; name=IDENTIFIER; EQUAL; BRACE_LEFT; fields=separated_list(COMMA, record_field); BRACE_RIGHT
    {
      L.record_type name fields (source $loc)
    }
;

%inline
record_field:
  | name=IDENTIFIER; COLON; typ=IDENTIFIER
    {
      (name, typ)
    }
;
