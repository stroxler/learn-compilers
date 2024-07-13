(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

let collect_snippet_file file_name =
  let snippet_name = Filename.(basename (chop_extension file_name)) in
  let channel = open_in file_name in
  let contents = really_input_string channel (in_channel_length channel) in
  Format.fprintf Format.std_formatter "let %s = %S@." snippet_name contents

let () =
  Sys.readdir (Sys.getcwd ())
  |> Array.iter (fun file_name ->
         if Filename.check_suffix file_name ".tig" then
           collect_snippet_file file_name )
