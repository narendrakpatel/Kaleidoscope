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