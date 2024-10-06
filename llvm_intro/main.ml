module Ast = struct
  type bop =
    | Add
    | Sub
    | Mul

  type expr =
    | Int of int
    | Var of string
    | Bop of bop * expr * expr
    | Call of string * expr list

  type define = Define of string * string list * expr
end


module Compiler = struct
    open Ast

    module Environment = Map.Make(String)

    let context = Llvm.create_context ()
    let md = Llvm.create_module context "arithmetic"
    let i32 = Llvm.i32_type context
    let builder = Llvm.builder context

    let op_to_llvm = function
      | Add -> Llvm.build_add
      | Sub -> Llvm.build_sub
      | Mul -> Llvm.build_mul

    let rec compile_expression environment = function
      | Int i -> Llvm.const_int i32 i
      | Var name -> Environment.find name environment
      | Bop (op, left, right) ->
         let left = compile_expression environment left in
         let right = compile_expression environment right in
         op_to_llvm op left right "" builder
      | Call (name, arguments) ->
         let arguments = List.map (compile_expression environment) arguments in
         (* This is kind of horrible, it's duplicated with compiling the define.
            Worse, there is no type safety: if you mess the type up here, you get
            a segfault... I thought it was the return type at first and ran into this. *)
         let define_type =
            let return_type = i32 in
            let parameter_types = Array.init (List.length arguments) (Fun.const i32) in
            Llvm.function_type return_type parameter_types
         in
         (match Llvm.lookup_function name md with
          | Some define ->
             print_endline "Just before build_call";
             Llvm.build_call define_type define (Array.of_list arguments) "" builder
          | None -> failwith ("Could not find funciton " ^ name)
         )



    let compile_define (Define (name, parameters, expression)) =
      let define_value =
        let define_type =
          let return_type = i32 in
          let parameter_types = Array.init (List.length parameters) (Fun.const i32) in
          Llvm.function_type return_type parameter_types
        in
        Llvm.define_function name define_type md
      in
      let environment =
        List.fold_left
          (fun (i, environment) x -> (i + 1, Environment.add x (Llvm.param define_value i) environment))
          (0, Environment.empty)
          parameters
        |> snd
      in
      let () =
        let current_block = Llvm.entry_block define_value in
        Llvm.position_at_end current_block builder
      in
      let () =
        let return_value = compile_expression environment expression in
        Llvm.build_ret return_value builder |> ignore;
      in
      ()

end


let functions: Ast.define list =
  [ Define ("foo",
            ["x"; "y"; "z"],
            Bop (Add, (Bop (Mul, Var "x", Var "y")), Var "z"))
  ; Define ("bar",
            ["x"],
            Bop (Sub, Var "x", Int 1))
  ; Define ("baz",
            ["z"],
            Call ("bar", [Var "z"]))
  ]


let main () =
  print_endline "Starting compiler";
  List.iter Compiler.compile_define functions;
  print_endline "Compiled, dumping output";
  print_endline @@ Llvm.string_of_llmodule Compiler.md


let () = main ()
