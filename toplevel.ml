(*===----------------------------------------------------------------------====
 * Top Level Parsing
 *===----------------------------------------------------------------------===*)

open Llvm
open Llvm_executionengine

(* top ::= definition | external | expression | ';' *)
let rec main_loop the_fpm the_execution_engine stream =
  match Stream.peek stream with
    | None -> ()

    (* Ignore semicolons *)
    | Some (Token.Kwd ';') ->
        Stream.junk stream;
        main_loop the_fpm the_execution_engine stream

    | Some token ->
        begin
          try match token with
          | Token.Def ->
              ignore(Parser.parse_definition stream)
              print_endline  "parsed a function definition"
              dump_value (Codegen.codegen_func e);
          | Token.Extern ->
              ignore(Parser.parse_extern stream)
              print_endline "parsed an extern"
              dump_value (Codegen.codegen_proto e);
          | _ ->
              (* evaluate a top-level expression into an anonymous function *)
              ignore(Parser.parse_toplevel stream)
              print_endline "parsed a top-level expression"
              let the_function = Codegen.func the_fpm e in;
              dump_value the_function;

              (* JIT the function, returns a function pointer *)
              let result = ExecutingEngine.run_function the_function [||]
                the_execution_engine in

              print_string "Evaluated to ";
              print_float (GenericValue.as_float Codegen.double_type result);
              print_newline ();
          with Stream.Error s ->
              (* skip token for error recovery *)
              Stream.junk stream;
              print_endline s;
        end
        print_string "ready> "; flush stdout;
        main_loop the_fpm the_execution_engine stream