open Ast
open Sast

(* Raw printer *)
let type_to_string = function
    TNum -> "TNUM"
  | TUnit -> "TUNIT"
  | TBool -> "TBOOL"
  | TString -> "TSTRING"
  | TChar -> "TCHAR"

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

let rec expression_to_string = function
    IntLiteral(i) -> string_of_int(i)
  | BoolLiteral(b) -> 
    if b then "TRUE"
    else "FALSE"
  | StringLiteral(s) -> s
  | CharLiteral(c) -> Char.escaped c
  | UnitLiteral -> "UNIT"
  | Binop(e1, op, e2) -> 
    "(" ^ expression_to_string e1 ^ ") " ^
    operation_to_string op ^ " " ^
    "(" ^ expression_to_string e2 ^ ")"
  | Uniop(op, e) ->
    "U" ^ operation_to_string op ^ " " ^
    "(" ^expression_to_string e ^ ")"
  | TypeAssing(id, e, t) ->
    "ASSIGN(" ^ type_to_string t ^ ") " ^ 
    id ^ " " ^
    expression_to_string e

let program_to_string (program : Ast.expression list) =
  "START\n" ^ String.concat "\n" (List.map expression_to_string program) ^ "\nEOF\n"

(* Compiler Exception *)
exception IllegalCharacter of char * int
exception IndentationError of int