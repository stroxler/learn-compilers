(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Soup

let pattern =
  Re.compile
    Re.(
      seq
        [ alt
            [ str "Fang_alloc"; str "Fang_asm"; str "Fang_flow"; str "Fang_frame"
            ; str "Fang_ir"; str "Fang_iter"; str "Fang_tiger"
            ; str "Fang_translate"; str "Fang_vm"; str "Fang_x64" ]; str "."
        ; group (rep1 (alt [wordc; str "."])) ])

let name_without_namespace text =
  match Re.exec_opt pattern text with
  | None -> None
  | Some group -> Some (Re.Group.all group).(1)

let fix_cross_reference node =
  match child node with
  | None -> ()
  | Some child -> (
    match texts child with
    | [text] -> (
      match name_without_namespace text with
      | None -> ()
      | Some short_name ->
          delete child ;
          create_text short_name |> append_child node )
    | _ -> () )

let () =
  let soup = read_channel stdin |> parse in
  soup $$ "a" |> iter fix_cross_reference ;
  soup |> to_string |> write_channel stdout
