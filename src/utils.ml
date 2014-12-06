open Ast

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
      "\n" ^ String.concat "|> " (List.map expression_to_string e_list) ^ ")\n"


    
let program_to_string (root : Ast.expression list) =
  "START\n" ^ String.concat "\n" (List.map expression_to_string root) ^ "\nEND\n"

(* Compiler Exception *)
exception IllegalCharacter of char * int
exception IndentationError of int
