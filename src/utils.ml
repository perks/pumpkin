open Ast
open Sast
open Parser

let operation_to_string = function
    Plus -> "PLUS"
  | Minus -> "MINUS"
  | Times -> "TIMES"
  | Divide -> "DIVIDE"
  | Modulo -> "MODULO"
  | Eq -> "EQ"
  | Neq -> "NEQ"
  | Gt -> "GT"
  | Lt -> "LT"
  | Gte -> "GTE"
  | Lte -> "LTE"
  | And -> "AND"
  | Or -> "OR"
  | Not -> "NOT"

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
  | AFuncPiping(e_list, t) ->
      "\n" ^ String.concat "|> " (List.map aexpression_to_string e_list) ^ "_" ^ s_type_to_string(t) ^ "\n"
  | ABlock(e_list, t) ->
    "\nBLOCK\n" ^ String.concat "\n" (List.map aexpression_to_string e_list) ^ "_" ^ s_type_to_string(t) ^ "\nENDBLOCK\n"

let s_program_to_string (aRoot : Sast.aExpression list) =
  "START SAST\n" ^ String.concat "\n" (List.map aexpression_to_string aRoot) ^ "\nEND SAST\n"

(* Raw printer *)
let type_to_string = function
    TInt -> "TINT"
  | TUnit -> "TUNIT"
  | TBool -> "TBOOL"
  | TString -> "TSTRING"
  | TChar -> "TCHAR"
  | TTuple  -> "TTUPLE"
  | TList -> "TLIST"
  | TFloat -> "TFLOAT"

let rec expression_to_string = function
    IntLiteral(i) -> string_of_int(i)
  | FloatLiteral(f) -> string_of_float(f)
  | BoolLiteral(b) ->
    if b then "TRUE"
    else "FALSE"
  | StringLiteral(s) -> s
  | CharLiteral(c) -> Char.escaped c
  | UnitLiteral -> "UNIT"
  | IdLiteral(id) -> id
  | Binop(e1, op, e2) ->
    expression_to_string e1 ^ " " ^
    operation_to_string op ^ " " ^
    expression_to_string e2
  | Unop(op, e) ->
    "U" ^ operation_to_string op ^ " " ^
    expression_to_string e
  | TypeAssing(id, e, t) ->
    "TASSIGN(" ^ type_to_string t ^ ") " ^
    id ^ " = " ^
    expression_to_string e
  | Assing(id, e) ->
    "ASSIGN " ^
    id ^ " = " ^
    expression_to_string e
  | TupleLiteral(e_list) ->
    "TUPLE(" ^ String.concat ", " (List.map expression_to_string e_list) ^ ")"
  | TupleAccess(e, e_acc) ->
    "(" ^ expression_to_string e ^ ")TUPLEACC(" ^ expression_to_string e_acc ^ ")"
  | ListLiteral(e_list) ->
    "List(" ^ String.concat ", " (List.map expression_to_string e_list) ^ ")"
  | Access(e, e_acc) ->
    "(" ^ expression_to_string e ^ ")ACCESS(" ^ expression_to_string e_acc ^ ")"
  | MapLiteral(map_list) ->
    let map_expression_tupal_to_string (e1, e2) =
      "(" ^ expression_to_string e1 ^ " -> " ^ expression_to_string e2 ^ ")"
    in
    "MAP(" ^ String.concat ", " (List.map map_expression_tupal_to_string map_list) ^ ")"
  | Block(e_list) ->
    "\nBLOCK" ^ String.concat "\n" (List.map expression_to_string e_list) ^ "\nENDBLOCK\n"
  | IfBlock(e, e_list) ->
    "\nIF(" ^ expression_to_string e ^ ")\n" ^
    "\t" ^ String.concat "\n\t" (List.map expression_to_string e_list) ^ "\n" ^
    "ENDIF\n"
  | IfElseBlock(e, e_list1, e_list2) ->
    "\nIF(" ^ expression_to_string e ^ ")\n" ^
    "\t" ^ String.concat "\n\t" (List.map expression_to_string e_list1) ^ "\n" ^
    "ELSE\n" ^
    "\t" ^ String.concat "\n\t" (List.map expression_to_string e_list2) ^ "\n" ^
    "ENDIF\n"
  | MatchBlock(e, match_list) ->
    let match_expression_tupal_to_string (e1, e2) =
      expression_to_string e1 ^ " => " ^ expression_to_string e2
    in
    expression_to_string e ^ " MATCH\n" ^
    "\t" ^ String.concat "\n\t| " (List.map match_expression_tupal_to_string match_list) ^ "\n" ^
    "ENDMATCH\n"
  | Parameter(id, t) ->
    id ^ " : " ^ type_to_string t
  | TypeFuncDecl(id, p_list, e_list, t) ->
    if (List.length p_list) <> 0 then
      "\ndef " ^ id ^ " (" ^ String.concat ", " (List.map expression_to_string p_list) ^ ") : " ^ type_to_string t ^ " =>\n" ^
      "\t" ^ String.concat "\n\t" (List.map expression_to_string e_list) ^ "\n"
    else
      "\ndef " ^ id ^ " : " ^ type_to_string t ^ " =>\n" ^
      "\t" ^ String.concat "\n\t" (List.map expression_to_string e_list) ^ "\n"
  | FuncDecl (id, p_list, e_list) ->
    if (List.length p_list) <> 0 then
      "\ndef " ^ id ^ " (" ^ String.concat ", " (List.map expression_to_string p_list) ^ ") =>\n" ^
      "\t" ^ String.concat "\n\t" (List.map expression_to_string e_list) ^ "\n"
    else
      "\ndef " ^ id ^ " =>\n" ^
      "\t" ^ String.concat "\n\t" (List.map expression_to_string e_list) ^ "\n"
  | FuncCall(id, p_list) ->
    if (List.length p_list) <> 0 then
      "\n" ^ id ^ " (" ^ String.concat ", " (List.map expression_to_string p_list) ^ ")\n"
    else
      "\n" ^ id ^ "()"
  | FuncPiping(e_list) ->
      "\n" ^ String.concat "|> " (List.map expression_to_string e_list) ^ "\n"
  | FuncComposition(e_list) ->
      "\n" ^ String.concat ">> " (List.map expression_to_string e_list) ^ "\n"
  | FuncAnon(p_list, e, t) ->
      "\n (" ^ String.concat ", " (List.map expression_to_string p_list) ^ " => " ^ expression_to_string e ^
        " ) : " ^ type_to_string t ^ "\n"
  | Wildcard -> "Wildcard"

let algebraic_params_to_string = function
    NativeParam(id, t) -> id ^ ": " ^ type_to_string t
  | AlgebraicParam(id, alg_t) -> id ^ ": " ^ alg_t

let algebraic_types_to_string = function
    AlgebraicBase(id, params) ->
      id ^ "(" ^ String.concat ", " (List.map algebraic_params_to_string params) ^ ")\n"
  | AlgebraicDerived(id, super, params) ->
      id ^ "(" ^ String.concat ", " (List.map algebraic_params_to_string params) ^ ")\n" ^
      " EXTENDS " ^ super ^"\n"

let program_to_string (expressions, algebraic_types) =
  "START-ALGDECLS\n" ^
  String.concat "\n" (List.map algebraic_types_to_string algebraic_types) ^
  "END-ALGDECLS\n" ^
  "START-PROG\n" ^
  String.concat "\n" (List.map expression_to_string expressions) ^
  "END-PROG\n"

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

(* Compiler Exception *)
exception IllegalCharacter of char * int
exception IndentationError of int