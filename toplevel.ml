(*===----------------------------------------------------------------------====
 * Top Level Parsing
 *===----------------------------------------------------------------------===*)

open Llvm

(* top ::= definition | external | expression | ';' *)
let rec main_loop stream =
  match Stream.peek stream with
    | None -> ()

    (* Ignore semicolons *)
    | Some (Token.Kwd ';') ->
        Stream.junk stream;
        main_loop stream

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
              dump_value (Codegen.codegen_func e);
          with Stream.Error s ->
              (* skip token for error recovery *)
              Stream.junk stream;
              print_endline s;
        end
        print_string "ready> "; flush stdout;
        main_loop stream