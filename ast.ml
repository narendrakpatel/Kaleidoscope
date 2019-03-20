(*===----------------------------------------------------------------------====
 * Abstract Syntax Tree
 *===----------------------------------------------------------------------===*)

(* Base type for all expression nodes *)
type expr =
  (* variant for all numeric literals *)
  | Number of float

  (* variant for referencing a variable *)
  | Variable of string

  (* variant for var/in *)
  | Var of (string * expr option) array * expr

  (* variant for a binary operator *)
  | Binary of char * expr * expr

  (* variant for a unary operator *)
  | Unary of char * expr

  (* variant for function calls *)
  | Call of string * expr array

  (* variant for if/then/else *)
  | If of expr * expr * expr

  (* variant for for/in *)
  | For of string * expr * expr * expr option * expr

(* This type represents the prototype for a function, which captures its name
 * and its arguments names (thus the number of arguments the function takes) *)
type proto =
  | Prototype of string * string array
  | BinOpPrototype of string * string array * int

(* This type represents a function definition itself *)
type func = Function of proto * expr