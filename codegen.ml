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
        | _ ->
            let callee = "binary" ^ (String.make 1 op) in
            let callee =
              match lookup_function callee the_module with
              | Some callee -> callee
              | None -> raise (Error "binary operator not found")
            in
            build_call callee [|lhs_val; rhs_val|] "binop" builder
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
  | Ast.If (c, t, e) ->
        let c = codegen_expr c in

        (* convery condition to a bool by comparing equal to 0.0 *)
        let zero = const_float double_type 0.0 in
        let cval = build_fcmp Fcmp.One c zero "ifc" builder in

        (* grab the first block so that we might later add the conditional
         * branch to it at the end of the function *)
        let start_bb = insertion_block builder in
        let the_function = block_parent start_bb in

        (* Emit 'then' value *)
        let then_bb = append_block context "then" the_function in
        position_at_end then_bb builder;
        let tval = codegen_expr t in

        (* codegen of 'then' can change the current block, update then_bb
         * for the phi. we create a new name because one is used for the
         * phi node, and the other is used for conditional branch *)
        let new_then_bb = insertion_block builder in

        (* Emit 'else' value *)
        let else_bb = append_block context "else" the_function in
        position_at_end else_bb builder;
        let eval = codegen_expr e in;

        (* codegen of 'else' can change the current block, update else_bb
         * for the phi *)
        let new_else_bb = insertion_block builder in

        (* Emit 'merge' block *)
        let merge_bb = append_block context "merge" the_function in
        position_at_end merge_bb builder;
        let incoming = [(tval, new_then_bb); (eval, new_else_bb)] in
        let phi = build_phi incoming "iftmp" builder in

        (* return to the start block to add the conditional branch *)
        position_at_end start_bb builder;
        ignore (build_cond_br cval then_bb else_bb builder);

        (* set an unconditional branch at the end of the 'then' block and
         * the 'else' block to the 'merge' block *)
        position_at_end new_then_bb builder;
        ignore (build_br merge_bb builder);
        position_at_end new_else_bb builder;
        ignore (build_br merge_bb builder);

        (* finally set the builder to the end of the merge block *)
        position_at_end merge_bb builder;

        phi
  | Ast.For (id, start, end_, step, body) ->
        (* Emit the start code first, without 'variable' in scope *)
        let start_val = codegen_expr start in

        (* make the new basic block for the loop header, inserting after
         * current block *)
         let preheader_bb = insertion_block builder in
         let the_function = block_parent preheader_bb in
         let loop_bb = append_block context "loop" the_function in

         (* insert an explicit fall through from the current block to
          * the loop_bb *)
          ignore (build_br loop_bb builder)

          (* start the insertion in loop_bb *)
          position_at_end loop_bb builder;

          (* start the phi node with an entry for start *)
          let variable = build_phi [(start_val, preheader_bb)] id builder in

          (* within the loop the variable is defined equal to the phi node
           * if it shadows an existing variable, we have to restore it,
           * so save it now *)
          let old_val =
            try Some (Hashtbl.find named_values id) with Not_found -> None
          in
          Hashtbl.add named_values id variable;

          (* Emit the body of the loop. this, like any other expr, can
           * change the current basic block. note that we ignore the value
           * computed by the body, but don't allow an error *)
          ignore (codegen_expr body)

          (* Emit the step value *)
          let step_val =
            match step with
            | Some step -> codegen_expr step

            (* if not specified, use 1.0 *)
            | None -> const_float double_type 1.0
          in

          let next_var = build_add variable step_val "nextvar" builder in

          (* compute the end condition *)
          let end_cond = codegen_expr end_ in

          (* convert condition to a bool by comparing equal to 0.0 *)
          let zero = const float_type double_type 0.0 in
          let end_cond = build_fcmp Fcmp.One end_cond zero "loopcond" builder in

          (* create the "after loop" block and insert it *)
          let loop_end_bb = insertion_block builder in
          let after_bb = append_block context "afterloop" the_function in

          (* insert the conditional branch into the end of the loop_end_bb *)
          ignore (build_cond_br end_cond loop_bb after_bb builder);

          (* any new code will be inserted in after_bb *)
          position_at_end after_bb builder;

          (* add a new entry to the phi node for the backedge *)
          add_incoming (next_var, loop_end_bb) variable;

          (* restore the unshadowed variable *)
          begin match old_val with
          | Some old_val -> Hashtbl.add named_values id old_val
          | None -> ()
          end;

          (* for expr always returns 0.0 *)
          const_null double_type

(* code generation for prototype
 * returns "Function*" instead of "Value*" *)
let codegen_proto = function
  | Ast.Prototype (name, args) ->
      (* make the function type: double(double...n times) *)
      let doubles = Array.make (Array.length args) double_type in
      let ft = function_type double_type doubles in
      let f =
        match lookup_function name the_module with
        | None -> declare_function name ft the_module
        | Some f ->
            (* if 'f' does not have body, it is forward declaration *)
            if block_begin f <> At_end f then
              raise (Error "redefinition of function")

            (* if 'f' took a different number of arguments, reject this*)
            if element_type (type_of f) <> ft then
              raise (Error "redefinition of function with different # args")
            f
      in

      (* set names for all arguments *)
      Array.iteri (fun i a ->
        let n = args.(i) in
        set_value_name n a;
        (* TODO: check for conflicting argument names *)
        Hashtbl.add named_values n a;
      ) (params f)
      f

let codegen_func the_fpm = function
  | Ast.Function (proto, body) ->
      Hashtbl.clear named_values;
      let the_function = codegen_proto proto in

      (* if this is an operator, install it *)
      begin match proto with
      | Ast.BinOpPrototype (name, args, prec) ->
          let op = name.[String.length name - 1] in
          Hashtbl.add Parser.binop_precedence op prec;
      | _ -> ()
      end;

      (* create a new basic block to start insertion into *)
      let basic_block = append_block context "entry" the_function in
      position_at_end basic_block builder

      try
        let ret_val = codegen_expr body in

        (* finish the function *)
        let _ = build_ret ret_value builder in

        (* validate the generated code, check for consistency *)
        Llvm_analysis.assert_valid_function the_function;

        (* Optimize the_function *)
        let _ = PassManager.run_function the_function the_fpm in

        the_function
      with e ->
        (* TODO: check for previously defined forward declaration *)
        delete_function the_function;
        raise e