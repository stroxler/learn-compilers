/* Copyright 2021 Jesse Haber-Kucharsky */
/* SPDX-License-Identifier: GPL-3.0-only */

/*
 * Keywords.
 */

%token AND
%token ARRAY
%token BREAK
%token DO
%token ELSE
%token END
%token FOR
%token FUNCTION
%token IF
%token IN
%token LET
%token NIL
%token OF
%token SIZE
%token THEN
%token TO
%token TYPE
%token VAR
%token WHILE

/*
 * Symbols.
 */

%token ASSIGN
%token BRACE_LEFT BRACE_RIGHT
%token BRACKET_LEFT BRACKET_RIGHT
%token COLON
%token COMMA
%token DOT
%token EQUAL NOT_EQUAL
%token FORWARD_SLASH
%token GREATER GREATER_EQUAL
%token LESS LESS_EQUAL
%token LOGICAL_AND
%token LOGICAL_OR
%token PAREN_LEFT PAREN_RIGHT
%token PLUS MINUS
%token SEMI
%token STAR

/*
 * Payloads.
 */

%token <int64> INT
%token <string> IDENTIFIER
%token <string> STRING

/*
 * Special.
 */

%token EOF

/*
 * Precedence rules.
 */

%nonassoc DO OF
%right THEN ELSE
%nonassoc ASSIGN
%nonassoc LOGICAL_AND LOGICAL_OR
%nonassoc EQUAL NOT_EQUAL LESS LESS_EQUAL GREATER GREATER_EQUAL
%left PLUS MINUS
%left STAR FORWARD_SLASH
%right NEGATE SIZE

%%
