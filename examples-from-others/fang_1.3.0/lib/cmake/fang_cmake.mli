(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

(** Tools for interacting with CMake. *)

open Bos

type system_error = [`Msg of string]

val create_project :
     ?tiger_compile_options:Cmd.t
  -> ?c_compile_options:Cmd.t
  -> ?link_options:Cmd.t
  -> ?rt_dir:Fpath.t
  -> ?build_dir:Fpath.t
  -> ?name:string
  -> Fpath.t
  -> (Fpath.t * string, system_error) result
(** [create_project path] defines a CMake project on the file-system for
    compiling the Tiger file with path [path]. The result is the path of the
    CMake build directory and the CMake project name.

    If everything is successful, when the resulting CMake project is built it
    produces an executable [$build_dir/$name].

    By default, the build directory is [$(working_dir)/_fangbuild] but may be
    selected via [build_dir].

    By default, the CMake project name is derived from the path of the Tiger
    file. For example, if the path is [my_tiger_file.tig] then the name will be
    [my_tiger_file].

    The CMake project needs to know the location of Fang's runtime source code.
    By default, the location is identified based on the path of the [fangc]
    executable. If this heuristic fails, then the location can be manually
    specified via [rt_dir].

    [tiger_compile_options] are options forwarded to [fangc] for compiling the
    Tiger source file into assembly.

    [c_compile_options] are are options forwarded to the C compiler for
    compiling Fang's runtime and the assembly file produced by [fangc].

    [link_options] are the options forwarded to the C compiler for linking the
    runtime library to the object file to produce an executable program. *)

val with_configure_cmd :
     ?cmake_options:Cmd.t
  -> build_dir:Fpath.t
  -> (Cmd.t -> ('a, system_error) result)
  -> ('a, system_error) result
(** [with_configure_cmd ~build_dir f] invokes [f] with a command that configures
    the CMake project with build-directory [build_dir]. *)

val with_build_cmd :
     build_dir:Fpath.t
  -> (Cmd.t -> ('a, system_error) result)
  -> ('a, system_error) result
(** [with_build_cmd ~build_dir f] invokes [f] with a command that builds the
    CMake project with build-directory [build_dir]. *)
