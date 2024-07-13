(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Bos

let ( let* ) r f = match r with Ok x -> f x | Error e -> Error e

let main cmake_options tiger_compile_options c_compile_options link_options
    rt_dir execute tiger_file () =
  match
    let* tiger_compile_options = Cmd.of_string tiger_compile_options in
    let* c_compile_options = Cmd.of_string c_compile_options in
    let* link_options = Cmd.of_string link_options in
    let* cmake_options = Cmd.of_string cmake_options in
    let cmake_options =
      Cmd.(
        cmake_options
        %% v
             ( "-DFangRuntime_COMPILE_OPTIONS="
             ^ String.concat ";" (to_list c_compile_options) )
        %% v
             ( "-DFangRuntime_LINK_OPTIONS="
             ^ String.concat ";" (to_list link_options) )) in
    let* build_dir, name =
      Fang_cmake.create_project ~tiger_compile_options ~c_compile_options
        ~link_options
        ?rt_dir:(Option.map Fpath.v rt_dir)
        (Fpath.v tiger_file)
    in
    let cmake_sink =
      match Logs.level () with
      | Some (Logs.Info | Debug) -> OS.Cmd.to_stdout
      | _ -> OS.Cmd.to_null in
    let* () =
      Fang_cmake.with_configure_cmd ~cmake_options ~build_dir (fun cmd ->
          OS.Cmd.(run_out cmd |> cmake_sink) )
    in
    let* () =
      Fang_cmake.with_build_cmd ~build_dir (fun cmd ->
          OS.Cmd.(run_out cmd |> cmake_sink) )
    in
    if execute then
      OS.Cmd.(run_out Cmd.(v (p Fpath.(build_dir / name))) |> to_stdout)
    else Ok ()
  with
  | Ok () -> ()
  | Error msg -> Logs.err (fun m -> m "%a" Rresult.R.pp_msg msg)

let initialize_logging style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (Logs_fmt.reporter ())

(* Command-line interface. *)

open Cmdliner

let logging =
  Term.(
    const initialize_logging $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let cmake_options =
  let doc = "Options forwarded to CMake during its invocation." in
  Arg.(value & opt string "" & info ["m"; "cmake-options"] ~docv:"OPTIONS" ~doc)

let tiger_compile_options =
  let doc = "Compilation options forwarded to the Tiger compiler." in
  Arg.(
    value & opt string ""
    & info ["t"; "tiger-compile-options"] ~docv:"OPTIONS" ~doc)

let c_compile_options =
  let doc = "Compilation options forwarded to the C compiler." in
  Arg.(
    value & opt string "" & info ["c"; "c-compile-options"] ~docv:"OPTIONS" ~doc)

let link_options =
  let doc = "Options forwarded to the linker." in
  Arg.(value & opt string "" & info ["l"; "link-options"] ~docv:"OPTIONS" ~doc)

let rt_dir =
  let doc = "The directory containing the Fang runtime sources." in
  Arg.(value & opt (some dir) None & info ["r"; "runtime-dir"] ~docv:"DIR" ~doc)

let tiger_file =
  let doc = "The path to a Tiger source file." in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"FILE" ~doc)

let execute =
  let doc = "Execute the program after it is compiled." in
  Arg.(value & flag & info ["execute"; "x"] ~doc)

let () =
  match
    Term.(
      eval
        ( const main $ cmake_options $ tiger_compile_options $ c_compile_options
          $ link_options $ rt_dir $ execute $ tiger_file $ logging
        , Term.info "fangu" ~doc:"Compile Tiger programs into executables." ))
  with
  | `Error _ -> exit 1
  | _ -> exit (if Logs.err_count () > 0 then 1 else 0)
