(* lexer_abap.ml *)
open Base

type token =
  | T_IDENTIFIER of string
  | T_KEYWORD of string
  | T_NUMBER of string
  | T_STRING of string
  | T_SYMBOL of string
  | T_NEWLINE
  | T_COMMENT of string
  | T_EOF
  | T_UNKNOWN of string

(* Extended list of ABAP keywords – sorted for clarity *)
let keywords =
  [ "IF"; "ELSE"; "ELSEIF"; "ENDIF";
    "CASE"; "WHEN"; "ENDCASE";
    "DO"; "WHILE"; "ENDDO"; "ENDWHILE";
    "LOOP"; "AT"; "ENDLOOP";
    "FOR"; "ENDFOR";
    "FUNCTION"; "ENDFUNCTION";
    "FORM"; "ENDFORM";
    "CLASS"; "ENDCLASS";
    "METHOD"; "ENDMETHOD";
    "DATA"; "CONSTANTS"; "PARAMETERS"; "TYPES"; "TABLES";
    "TRY"; "CATCH"; "ENDTRY";
    "SELECT"; "FROM"; "WHERE"; "SQL";
    "READ"; "PERFORM"; "CALL";
    "OPEN"; "CLOSE"; "DATASET";
    "UPDATE"; "INSERT"; "DELETE";
    "AUTHORITY-CHECK"; "AUTHENTICATE"; "OBJECT";
    "ENCRYPT"; "DECRYPT"; "HASH";
    "DEPRECATED_CRYPT"; "SXPG_COMMAND_EXECUTE";
    "SYSTEM"; "SUBMIT"; "EXEC";
    "BREAK-POINT"; "WATCHPOINT"; "DEBUG-POINT";
    "COMMIT"; "ROLLBACK";
    "PASSWORD"; "PASSWD"; "API_KEY"; "SECRET"; "TOKEN";
    "DB_CONNECTION"; "SAP_CONNECTION"; "ENCRYPTION_KEY"; "SEC_KEY";
    "INTO"; "CONCATENATE"; "DESTINATION";
    "OUTPUT"; "INPUT";
    "SY-SUBRC"; "SYST-SUBRC"
  ]
  |> List.map ~f:String.uppercase

let is_letter c = Char.is_alpha c
let is_digit c = Char.is_digit c
let is_whitespace c = Char.(c = ' ' || c = '\t' || c = '\r')

(* The tokenizer splits the source string into tokens.
   It returns comments as tokens (which may be dropped later).
*)
let tokenize (source: string) : token list =
  let length = String.length source in
  let rec lex pos tokens =
    if pos >= length then List.rev (T_EOF :: tokens)
    else
      let c = String.get source pos in
      if c = '\n' then
        lex (pos+1) (T_NEWLINE :: tokens)
      else if is_whitespace c then
        lex (pos+1) tokens
      else if (c = '*' || c = '"') &&
              (pos = 0 || (pos > 0 && String.get source (pos-1) = '\n')) then
        (* Comments: lines starting with '*' or '"' at the beginning *)
        let rec read_comment i =
          if i < length && String.get source i <> '\n' then read_comment (i+1)
          else i
        in
        let end_pos = read_comment pos in
        let comment_text = String.sub source ~pos ~len:(end_pos-pos) in
        lex end_pos (T_COMMENT comment_text :: tokens)
      else if is_letter c then
        let rec read_ident i =
          if i < length then
            let ch = String.get source i in
            if is_letter ch || is_digit ch || ch = '_' || ch = '-' then read_ident (i+1)
            else i
          else i
        in
        let end_pos = read_ident pos in
        let word = String.sub source ~pos ~len:(end_pos-pos) in
        let upword = String.uppercase word in
        let token =
          if List.mem ~equal:String.equal keywords upword then
            T_KEYWORD upword
          else
            T_IDENTIFIER word
        in
        lex end_pos (token :: tokens)
      else if is_digit c then
        let rec read_number i =
          if i < length then
            let ch = String.get source i in
            if is_digit ch then read_number (i+1) else i
          else i
        in
        let end_pos = read_number pos in
        let number = String.sub source ~pos ~len:(end_pos-pos) in
        lex end_pos (T_NUMBER number :: tokens)
      else if c = '\"' then
        (* Parse a string literal (no escape handling) *)
        let rec read_string i =
          if i < length then
            if String.get source i = '\"' then i+1 else read_string (i+1)
          else i
        in
        let end_pos = read_string (pos+1) in
        let str =
          if end_pos - pos - 2 >= 0 then
            String.sub source ~pos:(pos+1) ~len:(end_pos-pos-2)
          else ""
        in
        lex end_pos (T_STRING str :: tokens)
      else
        (* For any other character, return it as a symbol *)
        lex (pos+1) (T_SYMBOL (String.of_char c) :: tokens)
  in
  lex 0 []