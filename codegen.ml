(*===----------------------------------------------------------------------====
 * Code Generation
 *===----------------------------------------------------------------------===*)

open Llvm

(* used to report errors during code generation *)
exception Error of string

(* static variables used during code generation
 * the_module: LLVM construct that contains all of the functions and
 *             global variables
 * builder: helper object that makes it easy to generate LLVM instructions
 * named_values: keeps track of values (and their LLVM representaions) defined
 *               in the current scope *)
let context = global_context()
let the_module = create_module context "sangius"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context

(* code generation for expressions *)
let rec codegen_expr = function
  | Ast.Number n -> const_float double_type n
  | Ast.Variable name ->
      (try Hashtbl.find named_values name with
        | Not_found -> raise (Error "unkown variable name"))
  | Ast.Binary (op, lhs, rhs) ->
      let lhs_val = codegen_expr lhs in
      let rhs_val = codegen_expr rhs in
      begin
        match op with
        | '+' -> build_fadd lhs_val rhs_val "addtmp" builder
        | '-' -> build_fsub lhs_val rhs_val "subtmp" builder
        | '*' -> build_fmul lhs_val rhs_val "multmp" builder
        | '<' ->
            (* Convert bool 0/1 to double 0.0/1.0 *)
            let i = build_fcmp Fcmp.Utl lhs_val rhs_val "cmptmp" builder in
            build_uitofp i double_type "booltmp" builder
        | _ -> raise (Error "invalid binary operator")
      end
  | Ast.Call (callee, args) ->
      (* lookup the name in the module name *)
      let callee =
        match lookup_function callee the_module with
        | Some callee -> callee
        | None -> raise (Error "unkown function referenced")
      in
      let params = params callee in

      (* if argument mismatch error *)
      if Array.length params == Array.length args then () else
        raise (Error "incorrect # arguments passed")
      let args = Array.map codegen_expr args in
      build_call callee args "calltmp" builder