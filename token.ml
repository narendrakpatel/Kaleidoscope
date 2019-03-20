(*===----------------------------------------------------------------------====
 * Lexer tokens
 *===----------------------------------------------------------------------===*)

(*
 * Each token returned by the lexer includes a token code and potentially some metadata.
 * The lexer returns these 'Kwd' if it is an unknown character, otherwise one of these
 * others for known things. *)

type token =
  (* commands *)
  | Def | Extern

  (* primary *)
  | Ident of string | Number of float

  (* unknown *)
  | Kwd of char

  (* control *)
  | If | Then | Else
  | For | In

  (* operators *)
  | Binary
  | Unary

  (* variable definition *)
  | Var