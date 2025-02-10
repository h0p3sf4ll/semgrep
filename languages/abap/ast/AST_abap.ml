(* AST_abap.ml *)
open Base

type t =
  | Program of t list
  | Statement of string * t list         (* Generic statement with raw text and optional children *)
  | Expression of string
  | BinaryOp of string * t * t             (* Operator, left expression, right expression *)
  | Assignment of {                      (* New variant for assignments *)
        target: t;
        operator: string;
        source: t;
    }
  | IfStatement of {
      condition : t;
      then_branch : t list;
      elseif_branches : (t * t list) list;
      else_branch : t list option;
    }
  | CaseStatement of {
      expression : t;
      when_branches : (t * t list) list;
      else_branch : t list option;
    }
  | Loop of {
      loop_type : string;                  (* "DO" or "WHILE" *)
      condition : t;
      body : t list;
    }
  | LoopBlock of t list                    (* Generic LOOP ... ENDLOOP *)
  | LoopAt of {
      table : t;
      into : t option;
      body : t list;
    }
  | ForLoop of {
      iterator : t;
      collection : t;
      body : t list;
    }
  | FunctionDef of {
      def_type : string;                   (* "FUNCTION" or "FORM" *)
      name : string;
      parameters : t list;
      body : t list;
    }
  | ClassDef of {
      name : string;
      body : t list;
    }
  | MethodDef of {
      name : string;
      parameters : t list;
      body : t list;
    }
  | DataDeclaration of {
      name : string;
      datatype : string;
    }
  | ConstantDeclaration of {
      name : string;
      value : t;
    }
  | ParametersDeclaration of { declarations : t list }
  | TypesDeclaration of { declarations : t list }
  | TablesDeclaration of { declarations : t list }
  | TryCatch of {
      try_block : t list;
      catch_blocks : (string * t list) list;
    }
  | SelectStatement of {
      select_expr : t;
      from_clause : t;
      where_clause : t option;
    }
  | ReadTable of {
      table : t;
      key : t option;
    }
  | PerformStatement of {
      routine : string;
      using : t list option;
    }
  | CallFunction of {
      function_name : t;
      parameters : t list option;
      destination : t option;
    }
  | CallMethod of {
      method_name : t;
      parameters : t list option;
    }
  | WriteStatement of { expr : t }
  | CommitWork
  | RollbackWork
  | UpdateStatement of {
      table : t;
      set_clause : t option;
      where_clause : t option;
    }
  | InsertStatement of {
      table : t;
      values : t option;
    }
  | DeleteStatement of {
      table : t;
      where_clause : t option;
    }
  | CryptoStatement of {
      op : string;                         (* "ENCRYPT", "DECRYPT", "HASH" *)
      argument : t;
    }
  | DeprecatedCrypt of t                  (* DEPRECATED_CRYPT *)
  | SxpgCommandExecute of t               (* SXPG_COMMAND_EXECUTE *)
  | SqlStatement of string * t list       (* SQL commands *)
  | ConcatStatement of {
      sources : t list;
      destination : t;
    }                                      (* CONCATENATE ... INTO *)
  | SystemCommand of {
      command : t;                         (* SYSTEM, SUBMIT, EXEC, etc. *)
    }
  | FileAccessStatement of {
      action : string;                     (* "OPEN", "READ", "CLOSE" *)
      dataset : t;
      mode : string;                       (* "INPUT" or "OUTPUT" *)
    }
  | AuthorityCheck of {
      check_expr : t;
    }
  | Authenticate of {
      credentials : t;
    }
  | DebugStatement of {
      debug_cmd : string;                  (* "BREAK-POINT", "WATCHPOINT", "DEBUG-POINT" *)
    }
  | Unknown of string