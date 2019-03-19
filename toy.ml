(*===----------------------------------------------------------------------====
 * Main driver code
 *===----------------------------------------------------------------------===*)

let main () =
  (* Install standard binary operators
   * 1 is considered as lowest precedence *)
  Hashtbl.add Parser.binop_precedence '<' 10;
  Hashtbl.add Parser.binop_precedence '+' 20;
  Hashtbl.add Parser.binop_precedence '-' 20;
  Hashtbl.add Parser.binop_precedence '*' 40; (* highest*)

  (* process first token *)
  print_string "ready> "; flush stdout;
  let stream = Lexer.lex (Stream.of_channel stdin) in

  (* run the main "interpreter loop" *)
  Toplevel.main_loop stream;
;;

main()