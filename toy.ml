(*===----------------------------------------------------------------------====
 * Main driver code
 *===----------------------------------------------------------------------===*)

open Llvm
open Llvm_executionengine
open Llvm_target
open Llvm_scalar_opts

let main () =
  ignore (initialize_native_target ());
  (* Install standard binary operators
   * 1 is considered as lowest precedence *)
  Hashtbl.add Parser.binop_precedence '=' 2;
  Hashtbl.add Parser.binop_precedence '<' 10;
  Hashtbl.add Parser.binop_precedence '+' 20;
  Hashtbl.add Parser.binop_precedence '-' 20;
  Hashtbl.add Parser.binop_precedence '*' 40; (* highest*)

  (* process first token *)
  print_string "ready> "; flush stdout;
  let stream = Lexer.lex (Stream.of_channel stdin) in

  (* create the JIT *)
  let the_execution_engine = ExecutionEngine.create Codegen.the_module in

  (* function pass manager: per-function optimizations *)
  let the_fpm = PassManager.create_function Codegen.the_module in

  (* set up the optimizer pipeline
   * start with registering info about how the target lays out
   * data structures *)
  DataLayout.add (ExecutionEngine.target_data the_execution_engine) the_fpm;

  (* promote allocas to registers *)
  add_memory_to_register_promotion the_fpm;

  (* Do simple "peephole" optimizations and bit-twiddling optimization *)
  add_instruction_combining the_fpm;

  (* reassociate expressions *)
  add_reassociation the_fpm;

  (* eliminate Common SubExpressions *)
  add_gvn the_fpm;

  (* simplify the control flow graph eg: delete unreachable blocks *)
  add_cfg_simflication the_fpm;

  ignore (PassManager.initialize the_fpm);

  (* run the main "interpreter loop" *)
  Toplevel.main_loop the_fpm the_execution_engine stream;

  (* print out all the generated code *)
  dump_module Codegen.the_module
;;

main()