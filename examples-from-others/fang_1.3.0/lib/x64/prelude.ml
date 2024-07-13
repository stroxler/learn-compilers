(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_asm
open Fang_iter
open Fang_vm

let ax = box ()
and bx = box ()
and cx = box ()
and dx = box ()
and si = box ()
and di = box ()
and sp = box ()
and bp = box ()
and r8 = box ()
and r9 = box ()
and r10 = box ()
and r11 = box ()
and r12 = box ()
and r13 = box ()
and r14 = box ()
and r15 = box ()

module Registers = struct
  let all =
    Iter.adapt List.iter
      [ax; bx; cx; dx; si; di; sp; bp; r8; r9; r10; r11; r12; r13; r14; r15]

  let special = Iter.adapt List.iter [ax; sp; bp]
  let trashed = Iter.adapt List.iter [cx; dx; si; di; r8; r9; r10; r11]
  let preserved = Iter.adapt List.iter [bx; r12; r13; r14; r15]
end

open struct
  let reg_names =
    Box.Map.(
      empty |> add ax "rax" |> add bx "rbx" |> add cx "rcx" |> add dx "rdx"
      |> add si "rsi" |> add di "rdi" |> add sp "rsp" |> add bp "rbp"
      |> add r8 "r8" |> add r9 "r9" |> add r10 "r10" |> add r11 "r11"
      |> add r12 "r12" |> add r13 "r13" |> add r14 "r14" |> add r15 "r15")
end

let pp_reg ppf r =
  match Box.Map.find_opt r reg_names with
  | None -> Box.pp ppf r
  | Some s -> Fmt.(box (styled `Bold string)) ppf s

let pp_label = Label.pp ()

let jump_asm l =
  Asm.v (fun ~use:_ ~def:_ _ ppf () -> Fmt.pf ppf "@[<h>jmp@ %a@]" pp_label l)
