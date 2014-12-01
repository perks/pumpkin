open Ast

(* Raw printer *)
let type_to_string = function
    TInt -> "TINT"
  | TUnit -> "TUNIT"
  | TBool -> "TBOOL"
  | TString -> "TSTRING"
  | TChar -> "TCHAR"
  | TTuple  -> "TTUPAL"
  | TList -> "TLIST"

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
  | BoolLiteral(b) -> 
    if b then "TRUE"
    else "FALSE"
  | StringLiteral(s) -> s
  | CharLiteral(c) -> Char.escaped c
  | UnitLiteral -> "UNIT"
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
    "TUPAL(\n\t" ^ String.concat ",\n\t" (List.map expression_to_string e_list) ^ "\n)"
  | ListLiteral(e_list) ->
    "List(\n\t" ^ String.concat ",\n\t" (List.map expression_to_string e_list) ^ "\n)"
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
    
let program_to_string (root : Ast.expression list) =
  "START\n" ^ String.concat "\n" (List.map expression_to_string root) ^ "\nEND\n"

(* Compiler Exception *)
exception IllegalCharacter of char * int
exception IndentationError of int
