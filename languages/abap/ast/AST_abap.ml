(* AST_abap.ml *)
open Base

type ast =
  | Program of ast list
  | Statement of string * ast list  (* Generic statement with raw text and possible children *)
  | Expression of string
  | BinaryOp of string * ast * ast   (* Operator, left expression, right expression *)
  | IfStatement of {
      condition : ast;
      then_branch : ast list;
      elseif_branches : (ast * ast list) list;
      else_branch : ast list option;
    }
  | CaseStatement of {
      expression : ast;
      when_branches : (ast * ast list) list;
      else_branch : ast list option;
    }
  | Loop of {
      loop_type : string;  (* "DO" or "WHILE" *)
      condition : ast;
      body : ast list;
    }
  | LoopBlock of ast list                     (* LOOP ... ENDLOOP *)
  | LoopAt of {
      table : ast;
      into : ast option;
      body : ast list;
    }
  | ForLoop of {
      iterator : ast;
      collection : ast;
      body : ast list;
    }
  | FunctionDef of {
      def_type : string;   (* "FUNCTION" or "FORM" *)
      name : string;
      parameters : ast list;
      body : ast list;
    }
  | ClassDef of {
      name : string;
      body : ast list;
    }
  | MethodDef of {
      name : string;
      parameters : ast list;
      body : ast list;
    }
  | DataDeclaration of {
      name : string;
      datatype : string;
    }
  | ConstantDeclaration of {
      name : string;
      value : ast;
    }
  | ParametersDeclaration of { declarations : ast list }
  | TypesDeclaration of { declarations : ast list }
  | TablesDeclaration of { declarations : ast list }
  | TryCatch of {
      try_block : ast list;
      catch_blocks : (string * ast list) list;
    }
  | SelectStatement of {
      select_expr : ast;
      from_clause : ast;
      where_clause : ast option;
    }
  | ReadTable of {
      table : ast;
      key : ast option;
    }
  | PerformStatement of {
      routine : string;
      using : ast list option;
    }
  | CallFunction of {
      function_name : ast;
      parameters : ast list option;
      destination : ast option;
    }
  | CallMethod of {
      method_name : ast;
      parameters : ast list option;
    }
  | WriteStatement of { expr : ast }
  | CommitWork
  | RollbackWork
  | UpdateStatement of {
      table : ast;
      set_clause : ast option;
      where_clause : ast option;
    }
  | InsertStatement of {
      table : ast;
      values : ast option;
    }
  | DeleteStatement of {
      table : ast;
      where_clause : ast option;
    }
  | CryptoStatement of {
      op : string;         (* "ENCRYPT", "DECRYPT", "HASH" *)
      argument : ast;
    }
  | DeprecatedCrypt of ast                          (* DEPRECATED_CRYPT *)
  | SxpgCommandExecute of ast                       (* SXPG_COMMAND_EXECUTE *)
  | SqlStatement of string * ast list               (* SQL commands *)
  | ConcatStatement of {
      sources : ast list;
      destination : ast;
    }  (* CONCATENATE ... INTO *)
  | SystemCommand of {
      command : ast;       (* SYSTEM, SUBMIT, EXEC, etc. *)
    }
  | FileAccessStatement of {
      action : string;     (* "OPEN", "READ", "CLOSE" *)
      dataset : ast;
      mode : string;       (* "INPUT" or "OUTPUT", if specified *)
    }
  | AuthorityCheck of {
      check_expr : ast;
    }
  | Authenticate of {
      credentials : ast;
    }
  | DebugStatement of {
      debug_cmd : string;  (* "BREAK-POINT", "WATCHPOINT", "DEBUG-POINT" *)
    }
  | Unknown of string