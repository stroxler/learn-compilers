(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Bos

type system_error = [`Msg of string]

let pp_fn name ppf pp_args =
  Fmt.pf ppf "@[<h>%s@ (%a)@]" name Fmt.(box (concat ~sep:sp pp_args)) ()

let pp_named_arg name ppf pp_value =
  Fmt.pf ppf "@[<2>%s@ %a@]" name Fmt.(box pp_value) ()

let pp_cmakelists ?(tiger_compile_options = Cmd.empty)
    ?(c_compile_options = Cmd.empty) ?(link_options = Cmd.empty) ~rt_dir
    ~fangc_path ~tiger_file ppf name =
  let rt_dir =
    if Fpath.is_rel rt_dir then
      Fpath.(v "${CMAKE_CURRENT_SOURCE_DIR}" / ".." // rt_dir)
    else rt_dir in
  let tiger_file =
    if Fpath.is_rel tiger_file then
      Fpath.(v "${CMAKE_CURRENT_SOURCE_DIR}" / ".." // tiger_file)
    else tiger_file in
  let pp_version_requirement =
    Fmt.(
      const
        (pp_fn "cmake_minimum_required")
        [const (pp_named_arg "VERSION") (const string "3.13")]) in
  let pp_project =
    Fmt.(
      const (pp_fn "project")
        [ const string name
        ; const (pp_named_arg "LANGUAGES")
            (const string "ASM" ++ sp ++ const string "C") ]) in
  let pp_add_rt_dir =
    Fmt.(
      const (pp_fn "add_subdirectory")
        [const Fpath.pp rt_dir; const string "${CMAKE_CURRENT_BINARY_DIR}/rt"])
  in
  let asm_file = name ^ ".s" in
  let pp_compile =
    let cmd =
      Cmd.(
        v (p fangc_path)
        %% tiger_compile_options % "<" % p tiger_file % ">" % asm_file) in
    Fmt.(
      const
        (pp_fn "add_custom_command")
        [ const (pp_named_arg "OUTPUT") (const string asm_file)
        ; const (pp_named_arg "MAIN_DEPENDENCY") (const Fpath.pp tiger_file)
        ; const (pp_named_arg "DEPENDS") (const Fpath.pp fangc_path)
        ; const (pp_named_arg "COMMAND") (const Cmd.pp cmd) ]) in
  let pp_add_executable =
    Fmt.(
      const (pp_fn "add_executable") [const string name; const string asm_file])
  in
  let pp_c_compile_options =
    Fmt.(
      const
        (pp_fn "target_compile_options")
        [ const string name
        ; const (pp_named_arg "PUBLIC") (const Cmd.pp c_compile_options) ])
  in
  let pp_link_options =
    Fmt.(
      const
        (pp_fn "target_link_options")
        [ const string name
        ; const (pp_named_arg "PUBLIC") (const Cmd.pp link_options) ]) in
  let pp_link_libraries =
    Fmt.(
      const
        (pp_fn "target_link_libraries")
        [const string name; const string "fang_runtime"]) in
  Fmt.(
    vbox
      (concat ~sep:sp
         [ pp_version_requirement; pp_project; pp_add_rt_dir; pp_compile
         ; pp_add_executable; pp_c_compile_options; pp_link_options
         ; pp_link_libraries ] ))
    ppf ()

let ( let* ) = Stdlib.Result.bind

let create_build_dir () =
  let* cwd = OS.Dir.current () in
  let path = Fpath.(cwd / "_fangbuild") in
  let* _ = OS.Dir.create path in
  Ok path

let write_cmakelists ?tiger_compile_options ?c_compile_options ?link_options
    ~rt_dir ~fangc_path ~tiger_file ~name build_dir =
  let path = Fpath.(build_dir / "CMakeLists.txt") in
  let* r =
    OS.File.with_oc path
      (fun channel () ->
        let ppf = Format.formatter_of_out_channel channel in
        Fmt.pf ppf "%a@."
          (pp_cmakelists ?tiger_compile_options ?c_compile_options ?link_options
             ~rt_dir ~fangc_path ~tiger_file )
          name ;
        Ok () )
      ()
  in
  r

let find_fangc () = OS.Cmd.get_tool Cmd.(v "fangc")

let find_rt_dir fangc_path =
  let path = Fpath.(Fpath.parent fangc_path / ".." / "share" / "fang" / "rt") in
  let* path_exists = OS.Path.exists path in
  if not path_exists then
    Error
      (Rresult.R.msgf "%a:@ %a" Fmt.words
         "The inferred directory of Fang's runtime sources doesn't exist"
         Fpath.pp path )
  else Ok path

let create_project ?tiger_compile_options ?c_compile_options ?link_options
    ?rt_dir ?build_dir ?name tiger_file =
  let name =
    match name with
    | None -> Fpath.(rem_ext tiger_file |> basename)
    | Some name -> name in
  let* fangc_path = find_fangc () in
  let* build_dir =
    match build_dir with
    | None -> create_build_dir ()
    | Some build_dir -> Ok build_dir
  in
  let* rt_dir =
    match rt_dir with
    | None -> find_rt_dir fangc_path
    | Some rt_dir -> OS.Dir.must_exist rt_dir
  in
  let* () =
    write_cmakelists ?tiger_compile_options ?c_compile_options ?link_options
      ~rt_dir ~fangc_path ~tiger_file ~name build_dir
  in
  Ok (build_dir, name)

let with_configure_cmd ?(cmake_options = Cmd.empty) ~build_dir k =
  let* r =
    OS.Dir.with_current build_dir
      (fun () ->
        let* cmd = OS.Cmd.must_exist Cmd.(v "cmake" %% cmake_options % ".") in
        k cmd )
      ()
  in
  r

let with_build_cmd ~build_dir k =
  let* r =
    OS.Dir.with_current build_dir
      (fun () ->
        let* cmd =
          OS.Cmd.must_exist Cmd.(v "cmake" % "--build" % p build_dir)
        in
        k cmd )
      ()
  in
  r
