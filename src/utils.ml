open Ast
open Sast
open Parser

let operation_to_string = function
    Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "%"
  | Modulo -> "%"
  | Eq -> "is"
  | Neq -> "!="
  | Gt -> ">"
  | Lt -> "<"
  | Gte -> ">="
  | Lte -> "<="
  | And -> "and"
  | Or -> "or"
  | Not -> "not"

(* Ast printer *)
let type_to_string = function
    TInt -> "Int"
  | TUnit -> "Unit"
  | TBool -> "Bool"
  | TString -> "String"
  | TChar -> "Char"
  | TTuple  -> "Tuple"
  | TList -> "List"
  | TFloat -> "Float"

let rec expression_to_string indent_length = function
    IntLiteral(i) -> string_of_int(i)
  | FloatLiteral(f) -> string_of_float(f)
  | BoolLiteral(b) ->
    if b then "True"
    else "False"
  | StringLiteral(s) -> s
  | CharLiteral(c) -> Char.escaped c
  | UnitLiteral -> "^"
  | IdLiteral(id) -> id
  | Binop(e1, op, e2) ->
    expression_to_string indent_length e1 ^ " " ^
    operation_to_string op ^ " " ^
    expression_to_string indent_length e2
  | Unop(op, e) ->
    operation_to_string op ^ expression_to_string indent_length e
  | TypeAssing(id, e, t) ->
    "val " ^ id ^ ": " ^ type_to_string t ^ " = " ^ expression_to_string indent_length e
  | Assing(id, e) ->
    "val " ^ id ^ " = " ^ expression_to_string indent_length e
  | TupleLiteral(e_list) ->
    "(" ^ String.concat ", " (List.map (expression_to_string indent_length) e_list) ^ ")"
  | TupleAccess(e, e_acc) ->
    expression_to_string indent_length e ^ "$" ^ expression_to_string indent_length e_acc
  | ListLiteral(e_list) ->
    "[" ^ String.concat ", " (List.map (expression_to_string indent_length) e_list) ^ "]"
  | Access(e, e_acc) ->
    expression_to_string indent_length e ^ "." ^ expression_to_string indent_length e_acc
  | MapLiteral(map_list) ->
    let map_expression_tupal_to_string (e1, e2) =
      "(" ^ expression_to_string indent_length e1 ^ " -> " ^ expression_to_string indent_length e2 ^ ")"
    in
    "Map(" ^ String.concat ", " (List.map map_expression_tupal_to_string map_list) ^ ")"
  | IfBlock(e, e_list) ->
    let indent_length = indent_length + 1 in
    let tabs = String.make indent_length '\t' in
    "if " ^ expression_to_string indent_length e ^ " :\n" ^
    tabs ^ String.concat ("\n" ^ tabs) (List.map (expression_to_string indent_length) e_list)
  | IfElseBlock(e, e_list1, e_list2) ->
    let else_indent_length = indent_length and
    indent_length = indent_length + 1 in
    let else_tabs = String.make else_indent_length '\t'
    and tabs = String.make indent_length '\t' in
    "if " ^ expression_to_string indent_length e ^ " :\n" ^
    tabs ^ String.concat ("\n" ^ tabs) (List.map (expression_to_string indent_length) e_list1) ^ "\n" ^
    else_tabs ^"else :\n" ^
    tabs ^ String.concat ("\n" ^ tabs) (List.map (expression_to_string indent_length) e_list2)
  | MatchBlock(e, match_list) ->
    let match_expression_tupal_to_string (e1, e2) =
      expression_to_string indent_length e1 ^ " => " ^ expression_to_string indent_length e2
    in
    let indent_length = indent_length + 1 in
    let tabs = String.make indent_length '\t' in
    "match " ^ expression_to_string indent_length e ^ " :\n" ^
    tabs ^ "| " ^ String.concat ("\n" ^ tabs ^ "| ") (List.map match_expression_tupal_to_string match_list)
  | Parameter(id, t) ->
    id ^ " : " ^ type_to_string t
  | TypeFuncDecl(id, p_list, e_list, t) ->
    let indent_length = indent_length + 1 in
    let tabs = String.make indent_length '\t' in
    if (List.length p_list) <> 0 then
      "def " ^ id ^ " (" ^ String.concat ", " (List.map (expression_to_string indent_length) p_list) ^ ") : " ^ type_to_string t ^ " =>\n" ^
      tabs ^ String.concat ("\n" ^ tabs) (List.map (expression_to_string indent_length) e_list)
    else
      "def " ^ id ^ " : " ^ type_to_string t ^ " =>\n" ^
      tabs ^ String.concat ("\n" ^ tabs) (List.map (expression_to_string indent_length) e_list)
  | FuncDecl (id, p_list, e_list) ->
    let indent_length = indent_length + 1 in
    let tabs = String.make indent_length '\t' in
    if (List.length p_list) <> 0 then
      "def " ^ id ^ " (" ^ String.concat ", " (List.map (expression_to_string indent_length) p_list) ^ ") =>\n" ^
      tabs ^ String.concat ("\n" ^ tabs) (List.map (expression_to_string indent_length) e_list)
    else
      "def " ^ id ^ " =>\n" ^
      tabs ^ String.concat ("\n" ^ tabs) (List.map (expression_to_string indent_length) e_list)
  | FuncCall(id, p_list) ->
    if (List.length p_list) <> 0 then
      id ^ " (" ^ String.concat ", " (List.map (expression_to_string indent_length) p_list) ^ ")"
    else
      id ^ "()"
  | FuncPiping(e_list) ->
      String.concat "|> " (List.map (expression_to_string indent_length) e_list)
  | FuncComposition(e_list) ->
      String.concat ">> " (List.map (expression_to_string indent_length) e_list)
  | FuncAnon(p_list, e, t) ->
      "(" ^ String.concat ", " (List.map (expression_to_string indent_length) p_list) ^ " => " ^ expression_to_string indent_length e ^
        " ) : " ^ type_to_string t
  | Wildcard -> "_"

let algebraic_params_to_string = function
    NativeParam(id, t) -> id ^ ": " ^ type_to_string t
  | AlgebraicParam(id, alg_t) -> id ^ ": " ^ alg_t

let algebraic_variant_to_string = function
    VariantEmpty(id) -> id
  | VariantProduct(id, p_list) ->
      id ^ "(" ^ String.concat ", " (List.map algebraic_params_to_string p_list) ^ ")"

let algebraic_decls_to_string = function
    AlgebraicEmpty(id) ->
      "type " ^ id
  | AlgebraicProduct(id, p_list) ->
      "type " ^ id ^ "(" ^ String.concat ", " (List.map algebraic_params_to_string p_list) ^ ")"
  | AlgebraicSum(id, v_list) ->
      "type " ^ id ^ "=\n" ^
      "\t| " ^ String.concat "\n\t| " (List.map algebraic_variant_to_string v_list)

let program_to_string (expressions, algebraic_decls) =
  String.concat "\n" (List.map algebraic_decls_to_string algebraic_decls) ^
  String.concat "\n" (List.map (expression_to_string 0) expressions) ^ "\n"

(* Tokens to String *)

let token_to_string = function
    TERMINATOR -> "TERMINATOR" | INDENT -> "INDENT"
  | DEDENT -> "DEDENT" | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN" | COLON -> "COLON"
  | COMMA -> "COMMA" | LBRACK -> "LBRACK"
  | RBRACK -> "RBRACK" | TYPEARROW -> "TYPEARROW" | DEFARROW ->  "DEFARROW"
  | FPIPE -> "FPIPE" | BPIPE -> "BPIPE" | LCOMPOSE -> "LCOMPOSE" | RCOMPOSE -> "RCOMPOSE"
  | PLUS -> "PLUS" | MINUS -> "MINUS"
  | TIMES -> "TIMES" | DIVIDE -> "DIVIDE"
  | MODULO -> "MODULO" | EQ -> "EQ"
  | NEQ -> "NEQ" | GT -> "GT"
  | LT -> "LT" | GTE -> "GTE"
  | LTE -> "LTE" | AND -> "AND"
  | OR -> "OR" | NOT -> "NOT"
  | UMINUS -> "UMINUS" | UPLUS -> "UPLUS"
  | VAL -> "VAL" | ASSIGN -> "ASSIGN" | DEF -> "DEF"
  | IF -> "IF" | ELSE -> "ELSE"
  | TINT -> "TINT" | TUNIT -> "TUNIT"
  | TBOOL -> "TBOOL" | TSTRING -> "TSTRING"
  | TCHAR -> "TCHAR" | TTUPLE -> "TTUPLE"
  | TLIST -> "TLIST"| TFLOAT -> "TFLOAT"
  | TYPE -> "TYPE" | EXTENDS -> "EXTENDS"
  | TMAP -> "TMAP"
  | UNIT -> "UNIT"
  | MATCH -> "MATCH" | SELECTION -> "SELECTION" | WILDCARD -> "WILDCARD"
  | EOF -> "EOF" 
  | ID(s) -> "ID(" ^ s ^ ")"
  | INT(i) -> "INT(" ^ string_of_int i ^ ")"
  | FLOAT(f) -> "FLOAT(" ^ string_of_float f ^ ")"
  | DEDENT_COUNT(i) -> "DEDENT_COUNT(" ^ string_of_int i ^ ")"
  | BOOL(b) -> "BOOL(" ^ (if b then "true" else "false") ^ ")"
  | STRING(s) -> "STRING(" ^ s ^ ")"
  | TUPLEACC -> "TUPLEACC" | ACCESSOR -> "ACCESSOR"
  | CHAR(c) -> "CHAR(" ^ Char.escaped c ^ ")"
  | DEDENT_EOF(i) -> "DEDENT_EOF(" ^ string_of_int i ^ ")"

let token_list_to_string token_list =
  let rec helper last_line_number = function
      (token, line)::tail ->
        (if line != last_line_number then "\n" ^ string_of_int line ^ ". " else " ") ^
        token_to_string token ^ helper line tail
    | [] -> "\n"
  in helper 0 token_list

(* sast printer*)

let s_type_to_string = function
    Int -> "INT"
  | Unit -> "UNIT"
  | Bool -> "BOOL"
  | String -> "STRING"
  | Char -> "CHAR"
  | Tuple -> "TUPLE"
  | List -> "LIST"
  | Float -> "FLOAT"
  | Function -> "FUNCTION"
  | Map -> "MAP"
  | TAccess -> "TACCESS"
  | LMAccess -> "LMACCESS"

let rec aexpression_to_string = function
    AnIntLiteral(i, t) -> string_of_int(i) ^ "_" ^ s_type_to_string(t)
  | AFloatLiteral(f, t) -> string_of_float(f) ^ "_" ^ s_type_to_string(t)
  | ABinop(e1, op, e2, t) ->
    "( " ^ aexpression_to_string(e1) ^ " " ^
    operation_to_string(op) ^ " " ^
    aexpression_to_string(e2) ^ " " ^
    ")" ^ "_" ^ s_type_to_string(t)
  | AUnop(op, e1, t) ->
    operation_to_string(op) ^ " " ^
    aexpression_to_string(e1) ^ "_" ^
    s_type_to_string(t)
  | ABoolLiteral(b, t) ->
    if b then "TRUE" ^ "_" ^ s_type_to_string(t)
    else "FALSE" ^ "_" ^ s_type_to_string(t)
  | AStringLiteral(s, t) -> s ^ "_" ^ s_type_to_string(t)
  | ACharLiteral(c, t) -> Char.escaped c ^ "_" ^ s_type_to_string(t)
  | AUnit(t) -> "UNIT" ^ "_" ^ s_type_to_string(t)
  | AIdLiteral(id, t) -> id ^ "_" ^ s_type_to_string(t)
  | ATypeAssign(id, e, t) ->
    "TASSIGN(" ^ "_" ^ s_type_to_string t ^ ") " ^
    id ^ " = " ^
    aexpression_to_string e
  | ATupleLiteral(e_list, t) ->
    "(" ^ String.concat ", " (List.map aexpression_to_string e_list) ^ ")" ^ "_" ^ s_type_to_string(t)
  | ATupleAccess(e, e_acc, t)  ->
    "(" ^ aexpression_to_string e ^ ")TUPLEACC(" ^ aexpression_to_string e_acc ^ ")" ^ "_" ^ s_type_to_string(t)
  | AListLiteral(e_list, t) ->
    "LIST(" ^ String.concat ", " (List.map aexpression_to_string e_list) ^ ")" ^ "_" ^ s_type_to_string(t)
  | AListAccess(e_list, i, t) ->
    "LISTACCESS" ^ " " ^ string_of_int(i) ^ "_" ^ s_type_to_string(t)
  | AMapLiteral(map_list, t) ->
    let map_expression_tupal_to_string (e1, e2) =
      "(" ^ aexpression_to_string e1 ^ " -> " ^ aexpression_to_string e2 ^ ")"
    in
    "MAP(" ^ String.concat ", " (List.map map_expression_tupal_to_string map_list) ^ ")" ^ "_" ^ s_type_to_string(t)
  | AIfBlock(e, e_list, t) ->
    "\nIF(" ^ aexpression_to_string e ^ ")\n" ^
    "\t" ^ String.concat "\n\t" (List.map aexpression_to_string e_list) ^ "_" ^ s_type_to_string(t) ^ "\n" ^
    "ENDIF\n"
  | AIfElseBlock(e, e_list1, e_list2, t) ->
    "\nIF(" ^ aexpression_to_string e ^ ")\n" ^
    "\t" ^ String.concat "\n\t" (List.map aexpression_to_string e_list1) ^ "_" ^ s_type_to_string(t) ^ "\n" ^
    "ELSE\n" ^
    "\t" ^ String.concat "\n\t" (List.map aexpression_to_string e_list2) ^ "_" ^ s_type_to_string(t) ^ "\n" ^
    "ENDIF\n"
  | AStringChars(s, t) -> s ^ "_" ^ s_type_to_string(t)
  | AParameter(id, t) ->
    id ^ " : " ^ s_type_to_string t
  | ATypeFuncDecl(id, p_list, e_list, t) ->
    if (List.length p_list) <> 0 then
      "\n def " ^ id ^ " (" ^ String.concat ", " (List.map aexpression_to_string (List.rev p_list)) ^ ") : " ^ s_type_to_string t ^ " =>\n" ^
      "\t" ^ String.concat "\n\t" (List.map aexpression_to_string e_list) ^ "\n"
    else
      "\n def " ^ id ^ " : " ^ s_type_to_string t ^ " =>\n" ^
      "\t" ^ String.concat "\n\t" (List.map aexpression_to_string e_list) ^ "\n"
  | AFuncAnon(p_list, e_list, rt, t) -> 
    if (List.length p_list) <> 0 then
      "\n(" ^ String.concat ", " (List.map aexpression_to_string (List.rev p_list)) ^ s_type_to_string rt ^ " =>\n" ^
      "\t" ^ String.concat "\n\t" (List.map aexpression_to_string e_list) ^ ")\n"
    else
      "\n ( =>\n" ^
      "\t" ^ String.concat "\n\t" (List.map aexpression_to_string e_list) ^ ")\n"
  | AFuncCall(id, p_list, t) ->
    if (List.length p_list) <> 0 then
      "\n" ^ id ^ " (" ^ String.concat ", " (List.map aexpression_to_string p_list) ^ ")" ^ "_" ^ s_type_to_string(t) ^ "\n"
    else
      "\n " ^ id ^ "()" ^ "_" ^ s_type_to_string(t)
  | AFuncComposition(p_list, e_list, rt, t) ->
      "\n" ^ String.concat ">> " (List.map aexpression_to_string e_list) ^ "_" ^ s_type_to_string(rt) ^ "\n"
  | AFuncPiping(e_list, p_list, t) ->
      "\n" ^ String.concat "|> " (List.map aexpression_to_string e_list) ^ "_" ^ s_type_to_string(t) ^ "\n"

let s_program_to_string (aRoot : Sast.aExpression list) =
  "START SAST\n" ^ String.concat "\n" (List.map aexpression_to_string aRoot) ^ "\nEND SAST\n"