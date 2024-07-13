(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

type _ t = int Fmt.t

let pp_keyword ppf what = Fmt.(box (styled `Bold string)) ppf what
let pp_name ppf what = Fmt.(box string) ppf what
let pp_type_name ppf what = Fmt.(box (styled (`Fg `Blue) string)) ppf what

let pp_name_with_type type_name ppf name =
  Fmt.pf ppf "@[<h>%a:@ %a@]" pp_name name pp_type_name type_name

let int x _ ppf _ = Fmt.(box int64) ppf x
let string x _ ppf _ = Fmt.(box (styled (`Fg `Yellow) Dump.string)) ppf x
let nil _ ppf _ = Fmt.pf ppf "@[nil@]"

let arith oper lhs rhs _ ppf d =
  let oper =
    match oper with
    | `Add -> '+'
    | `Subtract -> '-'
    | `Multiply -> '*'
    | `Divide -> '/'
    | `And -> '&'
    | `Or -> '|' in
  let pp ppf () =
    Fmt.pf ppf "@[@[<h>%a@ %a@]@;<1 2>%a@]" lhs (d + 1) Fmt.char oper rhs (d + 1)
  in
  if d > 0 then (Fmt.parens pp) ppf () else pp ppf ()

let neg expr _ ppf d =
  let pp ppf () = Fmt.pf ppf "@[<h>-@,%a@]" expr (d + 1) in
  if d > 0 then (Fmt.parens pp) ppf () else pp ppf ()

let cmp rel lhs rhs _ ppf d =
  let rel =
    match rel with
    | `Equal -> "="
    | `Not_equal -> "<>"
    | `Less -> "<"
    | `Less_or_equal -> "<="
    | `Greater -> ">"
    | `Greater_or_equal -> ">=" in
  let pp ppf () =
    Fmt.pf ppf "@[@[<h>%a@ %a@]@;<1 2>%a@]" lhs (d + 1) Fmt.string rel rhs
      (d + 1) in
  if d > 0 then (Fmt.parens pp) ppf () else pp ppf ()

let value t _ = t

let if_ ~condition ~yes ?no _ ppf _ =
  match no with
  | None ->
      Fmt.pf ppf "@[<hv>@[<h>%a@ %a@ %a@]@;<1 2>%a@]" pp_keyword "if" condition
        0 pp_keyword "then" yes 0
  | Some no ->
      Fmt.pf ppf "@[<hv>@[<h>%a@ %a@ %a@]@;<1 2>%a@;%a@;<1 2>%a@]" pp_keyword
        "if" condition 0 pp_keyword "then" yes 0 pp_keyword "else" no 0

let while_ ~condition ~body _ ppf _ =
  Fmt.pf ppf "@[%a@;<1 2>%a@;<1 0>%a@;<1 2>%a@]" pp_keyword "while" condition 0
    pp_keyword "do" body 0

let for_ var_name ~initial ~final ~body _ ppf _ =
  Fmt.pf ppf
    "@[@[<h>%a@ %s@ @]:=@;<1 2>@[%a@;<1 0>%a@;<1 2>%a@]@;<1 0>%a@;<1 2>%a@]"
    pp_keyword "for" var_name initial 0 pp_keyword "to" final 0 pp_keyword "do"
    body 0

let break _ ppf _ = pp_keyword ppf "break"

let seq exprs _ ppf d =
  match exprs with
  | [expr] -> expr ppf d
  | _ -> Fmt.pf ppf "@[(@[<hv>%a)@]@]" (Fmt.(concat ~sep:semi) exprs) 0

let call name exprs _ ppf _ =
  Fmt.pf ppf "@[@[<h>%a(@]@;<0 2>@[<hv>%a@])@]" pp_name name
    (Fmt.(concat ~sep:comma) exprs)
    0

let uniform_array type_name ~size ~initial _ ppf _ =
  Fmt.pf ppf "@[@[%a[@;<0 2>%a@,]@]@;<1 2>@[%a@;<1 2>%a@]@]" pp_type_name
    type_name size 0 pp_keyword "of" initial 0

let array type_name values _ ppf _ =
  Fmt.pf ppf "@[%a@ %a@ [@;<0 2>@[%a@]@,]@]" pp_type_name type_name pp_keyword
    "of"
    Fmt.(concat ~sep:comma values)
    0

let size expr _ ppf d =
  let pp ppf () = Fmt.pf ppf "@[<h>%a@ %a@]" pp_keyword "size" expr (d + 1) in
  if d > 0 then Fmt.parens pp ppf () else pp ppf ()

let record type_name bindings _ ppf _ =
  let binding_pps =
    List.map
      (fun (name, value) ppf () ->
        Fmt.pf ppf "@[%a=@;<0 2>%a@]" pp_name name value 0 )
      bindings in
  Fmt.pf ppf "@[<hv>@[<h>%a@ @]{@;<0 2>@[%a@]@,}@]" pp_type_name type_name
    (Fmt.(concat ~sep:comma) binding_pps)
    ()

let scope decls value _ ppf _ =
  Fmt.pf ppf "@[<hv>%a@;<1 2>@[<v>%a@]@;<1 0>%a@;<1 2>%a@;<1 0>%a@]" pp_keyword
    "let" (Fmt.concat decls) 0 pp_keyword "in" value 0 pp_keyword "end"

let assign target expr _ ppf _ =
  Fmt.pf ppf "@[@[<h>%a@ @]:=@;<1 2>%a@]" target 0 expr 0

let name s _ ppf _ = pp_name ppf s
let index target i _ ppf _ = Fmt.pf ppf "@[%a[@;<0 2>%a@,]@]" target 0 i 0
let access target name _ ppf _ = Fmt.pf ppf "@[%a@;<0 2>.%s@]" target 0 name

let var name ?type_name value _ ppf _ =
  match type_name with
  | None ->
      Fmt.pf ppf "@[@[<h>%a@ %a@ @]:=@;<1 2>%a@]" pp_keyword "var" pp_name name
        value 0
  | Some type_name ->
      Fmt.pf ppf "@[@[<h>%a@ %a@ @]:=@;<1 2>%a@]" pp_keyword "var"
        (pp_name_with_type type_name)
        name value 0

let fns fs _ ppf _ =
  let sep ppf () = Fmt.pf ppf "@ %a " pp_keyword "and" in
  Fmt.pf ppf "@[%a@]" Fmt.(concat ~sep fs) 0

let fn name params ?type_name body _ ppf _ =
  let param_pps =
    List.map
      (fun (t, param_type_name) ppf () ->
        Fmt.pf ppf "@[<h>%a:@ %a@]" t 0 pp_type_name param_type_name )
      params in
  match type_name with
  | None ->
      Fmt.pf ppf "@[@[<h>%a@ %a(@;<0 4>@[<hv>%a@])@ @]=@;<1 2>%a@]" pp_keyword
        "function" pp_name name
        (Fmt.(concat ~sep:comma) param_pps)
        () body 0
  | Some type_name ->
      Fmt.pf ppf "@[@[<h>@[<h>%a@ %a(@;<0 4>@[<hv>%a@]):@ %a@]@ @]=@;<1 2>%a@]"
        pp_keyword "function" pp_name name
        (Fmt.(concat ~sep:comma) param_pps)
        () pp_type_name type_name body 0

let param name _ ppf _ = Fmt.pf ppf "@[%a@]" pp_name name

let types typs _ ppf _ =
  let sep ppf () = Fmt.pf ppf "@ %a " pp_keyword "and" in
  Fmt.pf ppf "@[%a@]" Fmt.(concat ~sep typs) 0

let alias_type ~name ~target _ ppf _ =
  Fmt.pf ppf "@[@[<h>%a@ %a@ =@]@;<1 2>%a@]" pp_keyword "type" pp_type_name name
    pp_type_name target

let array_type ~name ~item _ ppf _ =
  Fmt.pf ppf "@[@[<h>%a@ %a@ @]=@;<1 2>@[<h>%a@ %a@ %a@]@]" pp_keyword "type"
    pp_type_name name pp_keyword "array" pp_keyword "of" pp_type_name item

let record_type name fields _ ppf _ =
  let field_pps =
    List.map
      (fun (name, type_name) ppf () -> pp_name_with_type type_name ppf name)
      fields in
  Fmt.pf ppf "@[@[<h>%a@ %a@ @]=@;<1 2>{@[<v>%a@]}@]" pp_keyword "type"
    pp_type_name name
    (Fmt.(concat ~sep:comma) field_pps)
    ()

let pp ppf t = t ppf 0
