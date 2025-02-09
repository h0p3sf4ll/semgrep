(* Parse_abap.ml *)
(*
  This file merges the parser implementation and the external interface
  for the ABAP parser. It provides functions parse_source and to_tree that
  Semgrep uses to convert ABAP source code into an abstract syntax tree (AST).

  The implementation does not use Tree-sitter—everything is handled by custom code.
*)

open Base
open AST_abap
open Lexer_abap

(* --- Parser Infrastructure --- *)

type parser_state = {
  tokens : Lexer_abap.token list;
  mutable pos : int;
}

let current_token state =
  if state.pos < List.length state.tokens then
    List.nth_exn state.tokens state.pos
  else
    Lexer_abap.T_EOF

let advance state =
  state.pos <- state.pos + 1

let rec eat_newlines state =
  match current_token state with
  | Lexer_abap.T_NEWLINE | Lexer_abap.T_COMMENT _ ->
      advance state;
      eat_newlines state
  | _ -> ()

let expect_keyword state kw =
  match current_token state with
  | Lexer_abap.T_KEYWORD s when String.equal s (String.uppercase kw) ->
      advance state
  | tok ->
      failwith (Printf.sprintf "Expected keyword %s but found %s" kw
                  (match tok with
                   | Lexer_abap.T_KEYWORD s -> s
                   | Lexer_abap.T_IDENTIFIER s -> s
                   | Lexer_abap.T_NUMBER s -> s
                   | Lexer_abap.T_STRING s -> s
                   | Lexer_abap.T_SYMBOL s -> s
                   | Lexer_abap.T_NEWLINE -> "NEWLINE"
                   | Lexer_abap.T_COMMENT s -> "COMMENT(" ^ s ^ ")"
                   | Lexer_abap.T_EOF -> "EOF"
                   | Lexer_abap.T_UNKNOWN s -> s))

(* --- Expression Parsing --- *)

let rec parse_primary state : t =
  match current_token state with
  | Lexer_abap.T_IDENTIFIER id -> advance state; Expression id
  | Lexer_abap.T_NUMBER num -> advance state; Expression num
  | Lexer_abap.T_STRING s -> advance state; Expression ("\"" ^ s ^ "\"")
  | Lexer_abap.T_SYMBOL "(" ->
      advance state;
      let expr = parse_expression state in
      (match current_token state with
       | Lexer_abap.T_SYMBOL ")" -> advance state; expr
       | _ -> failwith "Expected closing parenthesis")
  | tok ->
      failwith (Printf.sprintf "Unexpected token in primary expression: %s"
                  (match tok with
                   | Lexer_abap.T_KEYWORD s -> s
                   | Lexer_abap.T_IDENTIFIER s -> s
                   | Lexer_abap.T_NUMBER s -> s
                   | Lexer_abap.T_STRING s -> s
                   | Lexer_abap.T_SYMBOL s -> s
                   | Lexer_abap.T_NEWLINE -> "NEWLINE"
                   | Lexer_abap.T_COMMENT s -> "COMMENT"
                   | Lexer_abap.T_EOF -> "EOF"
                   | Lexer_abap.T_UNKNOWN s -> s))

let operator_precedence op =
  match op with
  | "+" | "-" -> 1
  | "*" | "/" -> 2
  | "EQ" | "NE" -> 0  (* Relational operators: can be handled as needed *)
  | _ -> 0

let rec parse_binary_op_rhs state expr prec =
  let rec loop lhs =
    match current_token state with
    | Lexer_abap.T_SYMBOL op_str when List.mem ~equal:String.equal ["+"; "-"; "*"; "/"; "EQ"; "NE"] op_str ->
        let op_prec = operator_precedence op_str in
        if op_prec < prec then lhs
        else (
          advance state;
          let rhs = parse_primary state in
          let rec check_rhs current_rhs =
            match current_token state with
            | Lexer_abap.T_SYMBOL next_op when List.mem ~equal:String.equal ["+"; "-"; "*"; "/"; "EQ"; "NE"] next_op ->
                let next_prec = operator_precedence next_op in
                if next_prec > op_prec then parse_binary_op_rhs state current_rhs (op_prec + 1)
                else current_rhs
            | _ -> current_rhs
          in
          let rhs = check_rhs rhs in
          let new_lhs = BinaryOp (op_str, lhs, rhs) in
          loop new_lhs)
    | _ -> lhs
  in
  loop expr

and parse_expression state : t =
  let lhs = parse_primary state in
  parse_binary_op_rhs state lhs 0

(* --- Statement Parsing --- *)

let rec parse_statement state : t =
  eat_newlines state;
  match current_token state with
  | Lexer_abap.T_KEYWORD kw ->
      (match kw with
       | "IF" -> parse_if_statement state
       | "CASE" -> parse_case_statement state
       | "DO" | "WHILE" -> parse_loop_statement state
       | "LOOP" ->
           if (match current_token state with
               | Lexer_abap.T_KEYWORD s when String.equal s "AT" -> true
               | _ -> false)
           then parse_loop_at_statement state
           else parse_loop_block state
       | "FOR" -> parse_for_loop state
       | "FUNCTION" | "FORM" -> parse_function_def state
       | "CLASS" -> parse_class_def state
       | "METHOD" -> parse_method_def state
       | "DATA" -> parse_data_declaration state
       | "CONSTANTS" -> parse_constant_declaration state
       | "PARAMETERS" -> parse_parameters_declaration state
       | "TYPES" -> parse_types_declaration state
       | "TABLES" -> parse_tables_declaration state
       | "TRY" -> parse_try_catch state
       | "SELECT" -> parse_select_statement state
       | "READ" -> parse_read_table_statement state
       | "PERFORM" -> parse_perform_statement state
       | "CALL" -> parse_call_statement state
       | "WRITE" -> parse_write_statement state
       | "COMMIT" | "ROLLBACK" -> parse_commit_rollback_statement state
       | "UPDATE" -> parse_update_statement state
       | "INSERT" -> parse_insert_statement state
       | "DELETE" -> parse_delete_statement state
       | "ENCRYPT" | "DECRYPT" | "HASH" -> parse_crypto_statement state
       | "DEPRECATED_CRYPT" -> parse_deprecated_crypt state
       | "SXPG_COMMAND_EXECUTE" -> parse_sxpg_command_execute state
       | "SQL" -> parse_sql_statement state
       | "CONCATENATE" -> parse_concat_statement state
       | "SYSTEM" | "SUBMIT" | "EXEC" -> parse_system_command state
       | "OPEN" | "CLOSE" -> 
           parse_file_access_statement state (match current_token state with
                                              | Lexer_abap.T_KEYWORD s -> s
                                              | _ -> "UNKNOWN")
       | "AUTHORITY-CHECK" -> parse_authority_check state
       | "AUTHENTICATE" -> parse_authentication state
       | "BREAK-POINT" | "WATCHPOINT" | "DEBUG-POINT" -> parse_debug_statement state
       | _ -> parse_simple_statement state)
  | Lexer_abap.T_IDENTIFIER _ -> parse_simple_statement state
  | Lexer_abap.T_EOF -> Unknown "EOF"
  | _ -> parse_simple_statement state

and parse_simple_statement state : t =
  let rec gather_tokens acc =
    match current_token state with
    | Lexer_abap.T_NEWLINE | Lexer_abap.T_EOF -> acc
    | tok -> advance state; gather_tokens (acc @ [tok])
  in
  let tokens = gather_tokens [] in
  let text =
    List.map tokens ~f:(function
      | Lexer_abap.T_IDENTIFIER s -> s
      | Lexer_abap.T_KEYWORD s -> s
      | Lexer_abap.T_NUMBER s -> s
      | Lexer_abap.T_STRING s -> "\"" ^ s ^ "\""
      | Lexer_abap.T_SYMBOL s -> s
      | Lexer_abap.T_COMMENT s -> s
      | Lexer_abap.T_NEWLINE -> "\n"
      | Lexer_abap.T_EOF -> ""
      | Lexer_abap.T_UNKNOWN s -> s)
    |> String.concat ~sep:" " in
  Statement (text, [])

and parse_block state ~stop_keywords =
  let rec loop acc =
    match current_token state with
    | Lexer_abap.T_KEYWORD s when List.mem ~equal:String.equal stop_keywords s -> List.rev acc
    | Lexer_abap.T_EOF -> List.rev acc
    | _ -> let stmt = parse_statement state in loop (stmt :: acc)
  in
  loop []

(* --- Complex Constructs --- *)

and parse_if_statement state : t =
  expect_keyword state "IF";
  eat_newlines state;
  let condition = parse_expression state in
  eat_newlines state;
  let then_branch = parse_block state ~stop_keywords:["ELSEIF"; "ELSE"; "ENDIF"] in
  let rec gather_elseif acc =
    match current_token state with
    | Lexer_abap.T_KEYWORD s when String.equal s "ELSEIF" ->
        expect_keyword state "ELSEIF";
        eat_newlines state;
        let cond = parse_expression state in
        eat_newlines state;
        let branch = parse_block state ~stop_keywords:["ELSEIF"; "ELSE"; "ENDIF"] in
        gather_elseif (acc @ [(cond, branch)])
    | _ -> acc
  in
  let elseif_branches = gather_elseif [] in
  let else_branch =
    if (match current_token state with
        | Lexer_abap.T_KEYWORD s when String.equal s "ELSE" -> true
        | _ -> false)
    then (expect_keyword state "ELSE"; eat_newlines state; Some (parse_block state ~stop_keywords:["ENDIF"]))
    else None
  in
  expect_keyword state "ENDIF";
  IfStatement { condition; then_branch; elseif_branches; else_branch }

and parse_case_statement state : t =
  expect_keyword state "CASE";
  eat_newlines state;
  let expression = parse_expression state in
  eat_newlines state;
  let rec gather_when acc =
    match current_token state with
    | Lexer_abap.T_KEYWORD s when String.equal s "WHEN" ->
        expect_keyword state "WHEN";
        eat_newlines state;
        let when_expr = parse_expression state in
        eat_newlines state;
        let block = parse_block state ~stop_keywords:["WHEN"; "ELSE"; "ENDCASE"] in
        gather_when (acc @ [(when_expr, block)])
    | _ -> acc
  in
  let when_branches = gather_when [] in
  let else_branch =
    if (match current_token state with
        | Lexer_abap.T_KEYWORD s when String.equal s "ELSE" -> true
        | _ -> false)
    then (expect_keyword state "ELSE"; eat_newlines state; Some (parse_block state ~stop_keywords:["ENDCASE"]))
    else None
  in
  expect_keyword state "ENDCASE";
  CaseStatement { expression; when_branches; else_branch }

and parse_loop_statement state : t =
  let loop_type =
    match current_token state with
    | Lexer_abap.T_KEYWORD s when String.equal s "DO" || String.equal s "WHILE" -> s
    | _ -> failwith "Expected DO or WHILE"
  in
  advance state;
  eat_newlines state;
  let condition = if String.equal loop_type "WHILE" then parse_expression state else Expression "DO_LOOP" in
  eat_newlines state;
  let body = parse_block state ~stop_keywords:["ENDDO"; "ENDWHILE"] in
  (match current_token state with
   | Lexer_abap.T_KEYWORD s when String.equal s "ENDDO" || String.equal s "ENDWHILE" -> advance state
   | _ -> failwith "Expected termination of DO/WHILE loop");
  Loop { loop_type; condition; body }

and parse_loop_block state : t =
  expect_keyword state "LOOP";
  eat_newlines state;
  let body = parse_block state ~stop_keywords:["ENDLOOP"] in
  expect_keyword state "ENDLOOP";
  LoopBlock body

and parse_loop_at_statement state : t =
  expect_keyword state "LOOP";
  expect_keyword state "AT";
  eat_newlines state;
  let table_expr = parse_expression state in
  let into_expr =
    if (match current_token state with
        | Lexer_abap.T_KEYWORD s when String.equal s "INTO" -> true
        | _ -> false)
    then (expect_keyword state "INTO"; eat_newlines state; Some (parse_expression state))
    else None
  in
  eat_newlines state;
  let body = parse_block state ~stop_keywords:["ENDLOOP"] in
  expect_keyword state "ENDLOOP";
  LoopAt { table = table_expr; into = into_expr; body }

and parse_for_loop state : t =
  expect_keyword state "FOR";
  eat_newlines state;
  let iterator = parse_expression state in
  eat_newlines state;
  expect_keyword state "IN";
  eat_newlines state;
  let collection = parse_expression state in
  eat_newlines state;
  let body = parse_block state ~stop_keywords:["ENDFOR"] in
  expect_keyword state "ENDFOR";
  ForLoop { iterator; collection; body }

and parse_function_def state : t =
  let def_type =
    match current_token state with
    | Lexer_abap.T_KEYWORD s when String.equal s "FUNCTION" || String.equal s "FORM" -> s
    | _ -> failwith "Expected FUNCTION or FORM"
  in
  advance state;
  eat_newlines state;
  let name =
    match current_token state with
    | Lexer_abap.T_IDENTIFIER id -> (advance state; id)
    | _ -> failwith "Expected function/form name"
  in
  let parameters =
    if (match current_token state with
        | Lexer_abap.T_SYMBOL s when String.equal s "(" -> true
        | _ -> false)
    then (
      advance state;
      let rec gather_params acc =
        match current_token state with
        | Lexer_abap.T_SYMBOL s when String.equal s ")" -> (advance state; List.rev acc)
        | Lexer_abap.T_IDENTIFIER id -> (advance state; gather_params (acc @ [Expression id]))
        | Lexer_abap.T_SYMBOL s when String.equal s "," -> (advance state; gather_params acc)
        | _ -> failwith "Unexpected token in parameters list"
      in
      gather_params []
    ) else []
  in
  eat_newlines state;
  let body = parse_block state ~stop_keywords:(if String.equal def_type "FUNCTION" then ["ENDFUNCTION"] else ["ENDFORM"]) in
  (match current_token state with
   | Lexer_abap.T_KEYWORD s when String.equal s "ENDFUNCTION" || String.equal s "ENDFORM" -> advance state
   | _ -> failwith "Expected termination of function/form");
  FunctionDef { def_type; name; parameters; body }

and parse_class_def state : t =
  expect_keyword state "CLASS";
  eat_newlines state;
  let name =
    match current_token state with
    | Lexer_abap.T_IDENTIFIER id -> (advance state; id)
    | _ -> failwith "Expected class name"
  in
  eat_newlines state;
  let body = parse_block state ~stop_keywords:["ENDCLASS"] in
  expect_keyword state "ENDCLASS";
  ClassDef { name; body }

and parse_method_def state : t =
  expect_keyword state "METHOD";
  eat_newlines state;
  let name =
    match current_token state with
    | Lexer_abap.T_IDENTIFIER id -> (advance state; id)
    | _ -> failwith "Expected method name"
  in
  let parameters =
    if (match current_token state with
        | Lexer_abap.T_SYMBOL s when String.equal s "(" -> true
        | _ -> false)
    then (
      advance state;
      let rec gather_params acc =
        match current_token state with
        | Lexer_abap.T_SYMBOL s when String.equal s ")" -> (advance state; List.rev acc)
        | Lexer_abap.T_IDENTIFIER id -> (advance state; gather_params (acc @ [Expression id]))
        | Lexer_abap.T_SYMBOL s when String.equal s "," -> (advance state; gather_params acc)
        | _ -> failwith "Unexpected token in method parameters"
      in
      gather_params []
    ) else []
  in
  eat_newlines state;
  let body = parse_block state ~stop_keywords:["ENDMETHOD"] in
  expect_keyword state "ENDMETHOD";
  MethodDef { name; parameters; body }

and parse_data_declaration state : t =
  expect_keyword state "DATA";
  eat_newlines state;
  let name =
    match current_token state with
    | Lexer_abap.T_IDENTIFIER id -> (advance state; id)
    | _ -> failwith "Expected data identifier"
  in
  let datatype =
    if (match current_token state with
        | Lexer_abap.T_IDENTIFIER _ -> true
        | _ -> false)
    then (match current_token state with
          | Lexer_abap.T_IDENTIFIER dt -> (advance state; dt)
          | _ -> "UNKNOWN")
    else "UNKNOWN"
  in
  DataDeclaration { name; datatype }

and parse_constant_declaration state : t =
  expect_keyword state "CONSTANTS";
  eat_newlines state;
  let name =
    match current_token state with
    | Lexer_abap.T_IDENTIFIER id -> (advance state; id)
    | _ -> failwith "Expected constant identifier"
  in
  eat_newlines state;
  let value = parse_expression state in
  ConstantDeclaration { name; value }

and parse_parameters_declaration state : t =
  expect_keyword state "PARAMETERS";
  eat_newlines state;
  let rec gather_params acc =
    match current_token state with
    | Lexer_abap.T_NEWLINE | Lexer_abap.T_EOF -> List.rev acc
    | tok -> advance state; gather_params (acc @ [Expression (
        match tok with
        | Lexer_abap.T_IDENTIFIER s -> s
        | Lexer_abap.T_KEYWORD s -> s
        | Lexer_abap.T_NUMBER s -> s
        | Lexer_abap.T_STRING s -> "\"" ^ s ^ "\""
        | Lexer_abap.T_SYMBOL s -> s
        | _ -> ""
      )])
  in
  let declarations = gather_params [] in
  ParametersDeclaration { declarations }

and parse_types_declaration state : t =
  expect_keyword state "TYPES";
  eat_newlines state;
  let rec gather_types acc =
    match current_token state with
    | Lexer_abap.T_NEWLINE | Lexer_abap.T_EOF -> List.rev acc
    | tok -> advance state; gather_types (acc @ [Expression (
        match tok with
        | Lexer_abap.T_IDENTIFIER s -> s
        | Lexer_abap.T_KEYWORD s -> s
        | Lexer_abap.T_NUMBER s -> s
        | Lexer_abap.T_STRING s -> "\"" ^ s ^ "\""
        | Lexer_abap.T_SYMBOL s -> s
        | _ -> ""
      )])
  in
  let declarations = gather_types [] in
  TypesDeclaration { declarations }

and parse_tables_declaration state : t =
  expect_keyword state "TABLES";
  eat_newlines state;
  let rec gather_tables acc =
    match current_token state with
    | Lexer_abap.T_NEWLINE | Lexer_abap.T_EOF -> List.rev acc
    | tok -> advance state; gather_tables (acc @ [Expression (
        match tok with
        | Lexer_abap.T_IDENTIFIER s -> s
        | Lexer_abap.T_KEYWORD s -> s
        | Lexer_abap.T_NUMBER s -> s
        | Lexer_abap.T_STRING s -> "\"" ^ s ^ "\""
        | Lexer_abap.T_SYMBOL s -> s
        | _ -> ""
      )])
  in
  let declarations = gather_tables [] in
  TablesDeclaration { declarations }

and parse_try_catch state : t =
  expect_keyword state "TRY";
  eat_newlines state;
  let try_block = parse_block state ~stop_keywords:["CATCH"; "ENDTRY"] in
  let rec gather_catch acc =
    match current_token state with
    | Lexer_abap.T_KEYWORD s when String.equal s "CATCH" ->
        expect_keyword state "CATCH";
        eat_newlines state;
        let exception_type =
          match current_token state with
          | Lexer_abap.T_IDENTIFIER id -> (advance state; id)
          | _ -> "UNKNOWN_EXCEPTION"
        in
        eat_newlines state;
        let block = parse_block state ~stop_keywords:["CATCH"; "ENDTRY"] in
        gather_catch (acc @ [(exception_type, block)])
    | _ -> acc
  in
  let catch_blocks = gather_catch [] in
  expect_keyword state "ENDTRY";
  TryCatch { try_block; catch_blocks }

and parse_select_statement state : t =
  expect_keyword state "SELECT";
  eat_newlines state;
  let select_expr = parse_expression state in
  eat_newlines state;
  expect_keyword state "FROM";
  eat_newlines state;
  let from_clause = parse_expression state in
  eat_newlines state;
  let where_clause =
    if (match current_token state with
        | Lexer_abap.T_KEYWORD s when String.equal s "WHERE" -> true
        | _ -> false)
    then (expect_keyword state "WHERE"; eat_newlines state; Some (parse_expression state))
    else None
  in
  SelectStatement { select_expr; from_clause; where_clause }

and parse_read_table_statement state : t =
  expect_keyword state "READ";
  eat_newlines state;
  expect_keyword state "TABLE";
  eat_newlines state;
  let table_expr = parse_expression state in
  let key_expr =
    if (match current_token state with
        | Lexer_abap.T_KEYWORD s when String.equal s "WITH" -> true
        | _ -> false)
    then (
      expect_keyword state "WITH";
      eat_newlines state;
      if (match current_token state with
          | Lexer_abap.T_KEYWORD s when String.equal s "KEY" -> true
          | _ -> false)
      then (expect_keyword state "KEY"; eat_newlines state);
      Some (parse_expression state))
    else None
  in
  ReadTable { table = table_expr; key = key_expr }

and parse_perform_statement state : t =
  expect_keyword state "PERFORM";
  eat_newlines state;
  let routine =
    match current_token state with
    | Lexer_abap.T_IDENTIFIER id -> (advance state; id)
    | _ -> failwith "Expected routine name in PERFORM"
  in
  let using_params =
    if (match current_token state with
        | Lexer_abap.T_KEYWORD s when String.equal s "USING" -> true
        | _ -> false)
    then (
      expect_keyword state "USING";
      eat_newlines state;
      let rec gather acc =
        match current_token state with
        | Lexer_abap.T_NEWLINE | Lexer_abap.T_EOF -> List.rev acc
        | _ -> let expr = parse_expression state in gather (expr :: acc)
      in
      Some (gather []))
    else None
  in
  PerformStatement { routine; using = using_params }

and parse_call_statement state : t =
  expect_keyword state "CALL";
  eat_newlines state;
  (match current_token state with
   | Lexer_abap.T_KEYWORD s when String.equal s "FUNCTION" ->
       expect_keyword state "FUNCTION";
       eat_newlines state;
       let func_name = parse_expression state in
       let params =
         if (match current_token state with
             | Lexer_abap.T_KEYWORD s when String.equal s "EXPORTING" -> true
             | _ -> false)
         then (
           expect_keyword state "EXPORTING";
           eat_newlines state;
           let rec gather acc =
             match current_token state with
             | Lexer_abap.T_NEWLINE | Lexer_abap.T_EOF -> List.rev acc
             | _ -> let expr = parse_expression state in gather (expr :: acc)
           in
           Some (gather []))
         else None
       in
       let destination =
         if (match current_token state with
             | Lexer_abap.T_KEYWORD s when String.equal s "DESTINATION" -> true
             | _ -> false)
         then (expect_keyword state "DESTINATION"; eat_newlines state; Some (parse_expression state))
         else None
       in
       CallFunction { function_name = func_name; parameters = params; destination }
   | Lexer_abap.T_KEYWORD s when String.equal s "METHOD" ->
       expect_keyword state "METHOD";
       eat_newlines state;
       let method_name = parse_expression state in
       let params =
         if (match current_token state with
             | Lexer_abap.T_KEYWORD s when String.equal s "EXPORTING" -> true
             | _ -> false)
         then (
           expect_keyword state "EXPORTING";
           eat_newlines state;
           let rec gather acc =
             match current_token state with
             | Lexer_abap.T_NEWLINE | Lexer_abap.T_EOF -> List.rev acc
             | _ -> let expr = parse_expression state in gather (expr :: acc)
           in
           Some (gather []))
         else None
       in
       CallMethod { method_name = method_name; parameters = params }
   | _ -> failwith "Expected FUNCTION or METHOD after CALL")

and parse_write_statement state : t =
  expect_keyword state "WRITE";
  eat_newlines state;
  let expr = parse_expression state in
  WriteStatement { expr }

and parse_commit_rollback_statement state : t =
  match current_token state with
  | Lexer_abap.T_KEYWORD s when String.equal s "COMMIT" ->
      expect_keyword state "COMMIT";
      if (match current_token state with
          | Lexer_abap.T_KEYWORD s when String.equal s "WORK" -> true
          | _ -> false)
      then (expect_keyword state "WORK"; CommitWork)
      else CommitWork
  | Lexer_abap.T_KEYWORD s when String.equal s "ROLLBACK" ->
      expect_keyword state "ROLLBACK";
      if (match current_token state with
          | Lexer_abap.T_KEYWORD s when String.equal s "WORK" -> true
          | _ -> false)
      then (expect_keyword state "WORK"; RollbackWork)
      else RollbackWork
  | _ -> failwith "Expected COMMIT or ROLLBACK statement"

and parse_update_statement state : t =
  expect_keyword state "UPDATE";
  eat_newlines state;
  let table_expr = parse_expression state in
  let set_clause =
    if (match current_token state with
        | Lexer_abap.T_KEYWORD s when String.equal s "SET" -> true
        | _ -> false)
    then (expect_keyword state "SET"; eat_newlines state; Some (parse_expression state))
    else None
  in
  let where_clause =
    if (match current_token state with
        | Lexer_abap.T_KEYWORD s when String.equal s "WHERE" -> true
        | _ -> false)
    then (expect_keyword state "WHERE"; eat_newlines state; Some (parse_expression state))
    else None
  in
  UpdateStatement { table = table_expr; set_clause; where_clause }

and parse_insert_statement state : t =
  expect_keyword state "INSERT";
  eat_newlines state;
  let table_expr = parse_expression state in
  let values =
    if (match current_token state with
        | Lexer_abap.T_KEYWORD s when String.equal s "VALUES" -> true
        | _ -> false)
    then (expect_keyword state "VALUES"; eat_newlines state; Some (parse_expression state))
    else None
  in
  InsertStatement { table = table_expr; values }

and parse_delete_statement state : t =
  expect_keyword state "DELETE";
  eat_newlines state;
  let table_expr = parse_expression state in
  let where_clause =
    if (match current_token state with
        | Lexer_abap.T_KEYWORD s when String.equal s "WHERE" -> true
        | _ -> false)
    then (expect_keyword state "WHERE"; eat_newlines state; Some (parse_expression state))
    else None
  in
  DeleteStatement { table = table_expr; where_clause }

and parse_crypto_statement state : t =
  let op =
    match current_token state with
    | Lexer_abap.T_KEYWORD s when List.mem ~equal:String.equal ["ENCRYPT"; "DECRYPT"; "HASH"] s -> s
    | _ -> failwith "Expected ENCRYPT, DECRYPT, or HASH"
  in
  advance state;
  eat_newlines state;
  let arg = parse_expression state in
  CryptoStatement { op; argument = arg }

and parse_deprecated_crypt state : t =
  expect_keyword state "DEPRECATED_CRYPT";
  eat_newlines state;
  let arg = parse_expression state in
  DeprecatedCrypt arg

and parse_sxpg_command_execute state : t =
  expect_keyword state "SXPG_COMMAND_EXECUTE";
  eat_newlines state;
  let cmd = parse_expression state in
  SxpgCommandExecute cmd

and parse_sql_statement state : t =
  expect_keyword state "SQL";
  eat_newlines state;
  let rec gather_sql acc =
    match current_token state with
    | Lexer_abap.T_NEWLINE | Lexer_abap.T_EOF -> List.rev acc
    | tok ->
        let txt = (match tok with
                   | Lexer_abap.T_IDENTIFIER s | Lexer_abap.T_KEYWORD s | Lexer_abap.T_NUMBER s | Lexer_abap.T_SYMBOL s -> s
                   | Lexer_abap.T_STRING s -> "\"" ^ s ^ "\""
                   | _ -> "")
        in
        advance state;
        gather_sql (acc @ [txt])
  in
  let sql_cmd = String.concat ~sep:" " (gather_sql []) in
  SqlStatement (sql_cmd, [])

and parse_concat_statement state : t =
  expect_keyword state "CONCATENATE";
  eat_newlines state;
  let rec gather_sources acc =
    match current_token state with
    | Lexer_abap.T_KEYWORD s when String.equal s "INTO" -> List.rev acc
    | Lexer_abap.T_EOF -> List.rev acc
    | _ ->
        let expr = parse_expression state in
        gather_sources (expr :: acc)
  in
  let sources = gather_sources [] in
  expect_keyword state "INTO";
  eat_newlines state;
  let destination = parse_expression state in
  ConcatStatement { sources; destination }

and parse_system_command state : t =
  let cmd_kw =
    match current_token state with
    | Lexer_abap.T_KEYWORD s when List.mem ~equal:String.equal ["SYSTEM"; "SUBMIT"; "EXEC"] s -> s
    | _ -> failwith "Expected SYSTEM, SUBMIT, or EXEC"
  in
  advance state;
  eat_newlines state;
  let cmd = parse_expression state in
  SystemCommand { command = cmd }

and parse_file_access_statement state action : t =
  expect_keyword state action;
  eat_newlines state;
  expect_keyword state "DATASET";
  eat_newlines state;
  let dataset_expr = parse_expression state in
  let mode =
    if (match current_token state with
        | Lexer_abap.T_KEYWORD s when String.equal s "FOR" -> true
        | _ -> false)
    then (expect_keyword state "FOR";
          eat_newlines state;
          (match current_token state with
           | Lexer_abap.T_KEYWORD s when String.equal s "OUTPUT" || String.equal s "INPUT" ->
               let m = s in advance state; m
           | _ -> ""))
    else ""
  in
  FileAccessStatement { action; dataset = dataset_expr; mode }

and parse_authority_check state : t =
  expect_keyword state "AUTHORITY-CHECK";
  eat_newlines state;
  let check_expr = parse_expression state in
  AuthorityCheck { check_expr }

and parse_authentication state : t =
  expect_keyword state "AUTHENTICATE";
  eat_newlines state;
  let credentials = parse_expression state in
  Authenticate { credentials }

and parse_debug_statement state : t =
  let dbg_cmd =
    match current_token state with
    | Lexer_abap.T_KEYWORD s when List.mem ~equal:String.equal ["BREAK-POINT"; "WATCHPOINT"; "DEBUG-POINT"] s -> s
    | _ -> failwith "Expected a debug command"
  in
  advance state;
  DebugStatement { debug_cmd = dbg_cmd }

(* --- Entry Point --- *)

let parse_program source : t =
  let tokens = Lexer_abap.tokenize source in
  let state = { tokens; pos = 0 } in
  let rec parse_all acc =
    match current_token state with
    | Lexer_abap.T_EOF -> List.rev acc
    | _ ->
        let stmt = parse_statement state in
        parse_all (stmt :: acc)
  in
  let statements = parse_all [] in
  Program statements

(* --- External Interface --- *)

let parse_source ?(keep_all_comments=false) ~source =
  (* The keep_all_comments flag is not used; comments are dropped during lexing *)
  parse_program source

let to_tree ?(handle_errors=false) tree =
  Some tree