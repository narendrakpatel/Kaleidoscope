(*===----------------------------------------------------------------------====
 * Parser
 *===----------------------------------------------------------------------===*)

(* primary
 *  ::= identifier
 *  ::= numberexpr
 *  ::= parentexpr *)
let rec parse_primary = parser
  (* numberexpr ::= number *)
  | [< 'Token.Number n >] -> Ast.Number n

  (* parentexpr ::= '(' expression ')' *)
  | [< 'Token.Kwd '(';
        e = parse_expr;
        'Token.Kwd ')' ?? "Error: expected ')'" >] -> e

  (* identifier
   *  ::= identifier
   *  ::= identifier '(' argumentexpr ')' *)
  | [< 'Token.Ident id; stream >] ->
      (* TODO: modularise the code *)
      let rec parse_args accumulator = parser
        | [< e=parse_expr; stream >] ->
            begin parser
              | [< 'Token.Kwd ',';
                    e=parse_args (e :: accumulator) >] -> e
              | [< >] -> e :: accumulator
            end stream
        | [< >] -> accumulator
      in
      let rec parse_ident id = parser
        (* call *)
        | [< 'Token.Kwd '(';
              args=parse_args [];
              'Token.Kwd ')' ?? "Error: expected ')'" >] ->
            Ast.Call(id, Array.of_list(List.rev args))

        (* simple variable reference *)
        | [< >] -> Ast.Variable id
      in
      parse_ident id stream

  | [< >] -> raise (Stream.Error "unkwown token when expecting an expr")