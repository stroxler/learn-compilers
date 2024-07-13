(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_asm
open Fang_vm

type first = [`Entry of label | `Label of label]
type middle = [`Asm of asm]

type last =
  [ `Exit
  | `Cbranch of (label -> asm) * label * (label -> asm) * label
  | `Branch of (label -> asm) * label ]

type t = [`First of first | `Middle of middle | `Last of last]

let pp_asm pp_box = Fmt.quote (Asm.pp pp_box)

let pp_first ppf = function
  | `Entry l -> Fmt.pf ppf "@[<hv 2>(entry@ %a)@]" (Label.pp ()) l
  | `Label l -> Fmt.pf ppf "@[<hv 2>(label@ %a)@]" (Label.pp ()) l

let pp_middle pp_box ppf (`Asm s) =
  Fmt.pf ppf "@[<hv 2>(asm@ %a)@]" (pp_asm pp_box) s

let pp_last pp_box ppf = function
  | `Exit -> Fmt.pf ppf "@[(exit)@]"
  | `Cbranch (m, pos, m_inv, neg) ->
      Fmt.pf ppf "@[<hv 2>(cbranch@ %a@ %a)@]" (pp_asm pp_box) (m pos)
        (pp_asm pp_box) (m_inv neg)
  | `Branch (m, l) -> Fmt.pf ppf "@[<hv 2>(branch@ %a)@]" (pp_asm pp_box) (m l)

let pp pp_box ppf = function
  | `First first -> pp_first ppf first
  | `Middle middle -> pp_middle pp_box ppf middle
  | `Last last -> pp_last pp_box ppf last
