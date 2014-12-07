open Ast
open Sast

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
    "\nBLOCK\n" ^ String.concat "\n" (List.map expression_to_string e_list) ^ "\nENDBLOCK\n"
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
  | Parameter(id, t) ->
    id ^ " : " ^ type_to_string t
  | FuncDecl(id, p_list, e_list, t) ->
    if (List.length p_list) <> 0 then
      "\n def " ^ id ^ " (" ^ String.concat ", " (List.map expression_to_string p_list) ^ ") : " ^ type_to_string t ^ " =>\n" ^
      "\t" ^ String.concat "\n\t" (List.map expression_to_string e_list) ^ "\n"
    else
      "\n def " ^ id ^ " : " ^ type_to_string t ^ " =>\n" ^
      "\t" ^ String.concat "\n\t" (List.map expression_to_string e_list) ^ "\n"
  | FuncCall(id, p_list) ->
    if (List.length p_list) <> 0 then
      "\n" ^ id ^ " (" ^ String.concat ", " (List.map expression_to_string p_list) ^ ")\n"
    else
      "\n " ^ id ^ "()"
  | FuncPiping(e_list) ->
      "\n" ^ String.concat "|> " (List.map expression_to_string e_list) ^ "\n"
  | FuncComposition(e_list) ->
      "\n" ^ String.concat ">> " (List.map expression_to_string e_list) ^ "\n"
  | FuncAnon(p_list, e, t) ->
      "\n (" ^ String.concat ", " (List.map expression_to_string p_list) ^ " => " ^ expression_to_string e ^
        " ) : " ^ type_to_string t ^ "\n"

let rec aexpression_to_string = function
    AnIntLiteral(i, t) -> string_of_int(i) ^ s_type_to_string(t)
  | AFloatLiteral(f, t) -> string_of_float(f) ^ s_type_to_string(t)
  | ABinop(e1, op, e2, t) ->
    aexpression_to_string(e1) ^ " " ^
    operation_to_string(op) ^ " " ^
    aexpression_to_string(e1) ^ " " ^
    s_type_to_string(t)
  | AUnop(op, e1, t) ->
    operation_to_string(op) ^ " " ^
    aexpression_to_string(e1) ^ " " ^
    s_type_to_string(t)
  | ABoolLiteral(b, t) ->
    if b then "TRUE" ^ " " ^ s_type_to_string(t)
    else "FALSE" ^ " " ^ s_type_to_string(t)
  | AStringLiteral(s, t) -> s ^ " " ^ s_type_to_string(t)
  | ACharLiteral(c, t) -> Char.escaped c ^ " " ^ s_type_to_string(t)
  | AUnit(t) -> "UNIT" ^ " " ^ s_type_to_string(t)
  | AIdLiteral(id, t) -> id ^ " " ^ s_type_to_string(t)
  | ATypeAssign(id, e, t) ->
    "TASSIGN(" ^ s_type_to_string t ^ ") " ^
    id ^ " = " ^
    aexpression_to_string e
  | ATupleLiteral(e_list, t) ->
    "TUPLE(" ^ String.concat ", " (List.map aexpression_to_string e_list) ^ ") " ^ s_type_to_string(t)
  | ATupleAccess(e, e_acc, t) ->
    "(" ^ aexpression_to_string e ^ ")TUPLEACC(" ^ aexpression_to_string e_acc ^ ") " ^ s_type_to_string(t)
  | AListLiteral(e_list, t) ->
    "LIST(" ^ String.concat ", " (List.map aexpression_to_string e_list) ^ ") " ^ s_type_to_string(t)
  | AListAccess(e_list, i, t) ->
    "LISTACCESS" ^ " " ^ string_of_int(i) ^ " " ^ s_type_to_string(t)
  | AMapLiteral(map_list, t) ->
    let map_expression_tupal_to_string (e1, e2) =
      "(" ^ aexpression_to_string e1 ^ " -> " ^ aexpression_to_string e2 ^ ")"
    in
    "MAP(" ^ String.concat ", " (List.map map_expression_tupal_to_string map_list) ^ ") " ^ s_type_to_string(t)
  | AIfBlock(e, e_list, t) ->
    "\nIF(" ^ aexpression_to_string e ^ ")\n" ^
    "\t" ^ String.concat "\n\t" (List.map aexpression_to_string e_list) ^ " " ^ s_type_to_string(t) ^ "\n" ^
    "ENDIF\n"
  | AIfElseBlock(e, e_list1, e_list2, t) ->
    "\nIF(" ^ aexpression_to_string e ^ ")\n" ^
    "\t" ^ String.concat "\n\t" (List.map aexpression_to_string e_list1) ^ " " ^ s_type_to_string(t) ^ "\n" ^
    "ELSE\n" ^
    "\t" ^ String.concat "\n\t" (List.map aexpression_to_string e_list2) ^ " " ^ s_type_to_string(t) ^ "\n" ^
    "ENDIF\n"
  | AStringChars(s, t) -> s ^ " " ^ s_type_to_string(t)
  | AParameter(id, t) ->
    id ^ " : " ^ s_type_to_string t
  | AFuncDecl(id, p_list, e_list, t) ->
    if (List.length p_list) <> 0 then
      "\n def " ^ id ^ " (" ^ String.concat ", " (List.map aexpression_to_string p_list) ^ ") : " ^ s_type_to_string t ^ " =>\n" ^
      "\t" ^ String.concat "\n\t" (List.map aexpression_to_string e_list) ^ "\n"
    else
      "\n def " ^ id ^ " : " ^ s_type_to_string t ^ " =>\n" ^
      "\t" ^ String.concat "\n\t" (List.map aexpression_to_string e_list) ^ "\n"
  | AFuncCall(id, p_list, t) ->
    if (List.length p_list) <> 0 then
      "\n" ^ id ^ " (" ^ String.concat ", " (List.map aexpression_to_string p_list) ^ ") " ^ s_type_to_string(t) ^ "\n"
    else
      "\n " ^ id ^ "() " ^ s_type_to_string(t)
  | AFuncComposition(e_list, t) ->
      "\n" ^ String.concat ">> " (List.map aexpression_to_string e_list) ^ " " ^ s_type_to_string(t) ^ "\n"
  | AFuncPiping(e_list, t) ->
      "\n" ^ String.concat "|> " (List.map aexpression_to_string e_list) ^ " " ^ s_type_to_string(t) ^ "\n"
  | ABlock(e_list, t) ->
    "\nBLOCK\n" ^ String.concat "\n" (List.map aexpression_to_string e_list) ^ " " ^ s_type_to_string(t) ^ "\nENDBLOCK\n"

let program_to_string (root : Ast.expression list) =
  "START\n" ^ String.concat "\n" (List.map expression_to_string root) ^ "\nEND\n"

let s_program_to_string (aRoot : Sast.aExpression list) =
  "START SAST\n" ^ String.concat "\n" (List.map aexpression_to_string aRoot) ^ "\nEND SAST\n"

(* Compiler Exception *)
exception IllegalCharacter of char * int
exception IndentationError of int
