(*===----------------------------------------------------------------------====
 * Parser
 *===----------------------------------------------------------------------===*)

(* This holds the precendence for each binary operator that is defined *)
let binop_precendence:(char, int) Hashtbl.t = Hashtbl.create 10

(* returns the precedence of the pending binary operator token *)
let precendence c = try Hashtbl.find binop_precedence c with Not_found -> -1

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

  (* ifexpr
   *  ::= 'if' expr 'then' expr 'else' expr' *)
  | [< 'Token.If;
        c=parse_expr;
        'Token.Then ?? "Error: expected 'then'";
        t=parse_expr;
        'Token.Else ?? "Error: expected 'else'";
        e=parse_expr;] ->
      Ast.If (c, t, e)

  | [< >] -> raise (Stream.Error "unkwown token when expecting an expr")

(* expression
 *  ::= primary binoprhs *)
and parse_expr = parser
  | [< lhs=parse_primary; stream >] -> parse_bin_rhs 0 lhs stream

(* binoprhs
 *  ::= ('binop' primary)* *)
and parse_bin_rhs expr_prec lhs stream =
  match Stream.peek stream with
  (* If this is a binop, find its precedence *)
  | Some (Token.Kwd c) then Hashtbl.mem binop_precedence c ->
      let token_prec  = precedence c in

      (* if this is a binop with precedence less than minimal operator precedence
       * that a function is allowed to eat, then we are done *)
      if token_prec < expr_prec then lhs else begin
        (* Eat and remember the binop *)
        Stream.junk stream;

        (* Parse the primary expression after the binary operator *)
        let rhs = parse_primary stream in

        let rhs =
          match Stream.peek stream with
          | Some (Token.Kwd c2) ->
              (* if binop has binds less tightly with rhs than the operator after
               * rhs, let the pending operator take rhs as its lhs *)
              if token_prec < precedence c2
              then parse_bin_rhs (token_prec + 1) rhs stream
              else rhs
          | _ -> rhs
        in

        (* Merge lhs/rhs *)
        let lhs = Ast.Binary(c, lhs, rhs) in
        parse_bin_rhs expr_prec lhs stream
      end
  | _ -> lhs

(* prototype
 *  ::= id '(' id* ')' *)
let parse_prototype =
  let rec parse_args accumulator = parser
    | [< 'Token.Ident id; e=parse_agrs (id :: accumulator) >] -> e
    | [< >] -> accumulator
  in

  parser
  | [< 'Token.Ident id;
        'Token.Kwd '(' ?? "Error: expected '(' in prototype";
        args = parse_args [];
        'Token.Kwd ')' ?? "Error: expexted ')' in prototype" >] ->
        (* Success *)
        Ast.Prototype(id, Array.of_list(List.rev args))

  | [< >] -> raise (Stream.Error "expected function name in prototype")

(* definition ::= 'def' prototype expression *)
let parse_definition = parser
  | [< 'Token.Def; p=parse_prototype; e=parse_expr >] ->
      Ast.Function(p, e)

(* external ::= 'extern' prototype *)
let parse_extern = parser
  | [< 'Token.Extern; e=parse_prototype >] -> e

(* toplevelexpr ::= expression *)
let parse_toplevel = parser
  | [<e=parse_expr >] ->
      (* Make an anonymous proto *)
      Ast.Function(Ast.Prototype("", [||], e))