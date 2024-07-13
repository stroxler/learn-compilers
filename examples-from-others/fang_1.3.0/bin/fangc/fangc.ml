(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang

type emit = Tiger | IR | Canonical_ir | Flow | Allocated_flow | Asm

let main unsafe emit () =
  let lexbuf = Lexing.from_channel ~with_positions:true stdin in
  let safe = not unsafe in
  let compile =
    match emit with
    | Tiger -> compile_to_tiger
    | IR -> compile_to_ir ~safe
    | Canonical_ir -> compile_to_canonical_ir ~safe
    | Flow -> compile_to_flow ~safe
    | Allocated_flow -> compile_to_allocated_flow ~safe
    | Asm -> compile_to_asm ~safe in
  match compile lexbuf with
  | Ok pp -> Logs.app (fun m -> m "%a" pp ())
  | Error e -> Logs.err (fun m -> m "%a" pp_error e)

let initialize_logging style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (Logs_fmt.reporter ())

(* Command-line interface. *)

open Cmdliner

let logging =
  Term.(
    const initialize_logging $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let unsafe =
  let doc =
    "Omit safety checks for accessing nil records and arrays out of bounds."
  in
  Arg.(value & flag & info ["u"; "unsafe"] ~doc)

let emit =
  let doc = "The output format of the compilation process." in
  Arg.(
    value
    & opt
        (enum
           [ ("tiger", Tiger); ("ir", IR); ("canonical-ir", Canonical_ir)
           ; ("flow", Flow); ("allocated-flow", Allocated_flow); ("asm", Asm) ] )
        Asm
    & info ["emit"; "e"] ~docv:"FORMAT" ~doc)

let () =
  match
    Term.(
      eval
        ( const main $ unsafe $ emit $ logging
        , Term.info "fangc"
            ~doc:"Compile Tiger programs into alternative representations." ))
  with
  | `Error _ -> exit 1
  | _ -> exit (if Logs.err_count () > 0 then 1 else 0)
