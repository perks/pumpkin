open Ast
open Sast
open Parser

(* Ast Printer *)
let operation_to_string = function
    Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
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
  | Cons -> "::"

let rec type_to_string = function
    TInt -> "Int"
  | TUnit -> "Unit"
  | TBool -> "Bool"
  | TString -> "String"
  | TChar -> "Char"
  | TFloat -> "Float"
  | TTuple(t)  -> "Tuple[" ^ String.concat ", " (List.map type_to_string t) ^ "]"
  | TList(t)-> "List[" ^ type_to_string t ^ "]"
  | TMap(t1, t2) -> "Map[" ^ type_to_string t1 ^ ", " ^ type_to_string t2 ^ "]"
  | TFunction(t1, t2) -> "(" ^ String.concat ", " (List.map type_to_string t1) ^ " => " ^ type_to_string t2 ^ ")"

let parameters_to_string (id, t) = id ^ ": " ^ type_to_string t

let rec expression_to_string indent_length = function
    IntLiteral(i) -> string_of_int(i)
  | FloatLiteral(f) -> string_of_float(f)
  | BoolLiteral(b) ->
      if b then "True"
      else "False"
  | StringLiteral(s) -> s
  | CharLiteral(c) -> "'" ^ Char.escaped c ^ "'"
  | UnitLiteral -> "()"
  | IdLiteral(id) -> id
  | Binop(e1, op, e2) ->
      "(" ^
      expression_to_string indent_length e1 ^ " " ^
      operation_to_string op ^ " " ^
      expression_to_string indent_length e2 ^
      ")"
  | Unop(op, e) ->
    "(" ^ operation_to_string op ^
    (
      match op with
          Not ->  " "
        | _   ->  ""
    ) ^ expression_to_string indent_length e ^ ")"
  | TypedAssign(id, e, t) ->
    "val " ^ id ^ ": " ^ type_to_string t ^ " = " ^ expression_to_string indent_length e
  | Assign(id, e) ->
    "val " ^ id ^ " = " ^ expression_to_string indent_length e
  | Reassign(id, e) ->
    id ^ " = " ^ expression_to_string indent_length e
  | TupleLiteral(e_list) ->
    "(" ^ String.concat ", " (List.map (expression_to_string indent_length) e_list) ^ ",)"
  | TupleAccess(e, e_acc) ->
    expression_to_string indent_length e ^ "$(" ^ expression_to_string indent_length e_acc ^ ")"
  | ListLiteral(e_list) ->
    "[" ^ String.concat ", " (List.map (expression_to_string indent_length) e_list) ^ "]"
  | ListAccess(e, e_acc) ->
    expression_to_string indent_length e ^ "[" ^ expression_to_string indent_length e_acc ^ "]"
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
  | Call(exp, e_list) ->
      (expression_to_string indent_length exp) ^ 
      ( 
        match e_list with
           UnitLiteral::[] -> "()"
         | _ -> "(" ^ String.concat ", " (List.map (expression_to_string indent_length) e_list) ^ ")"
      )
  | TypedFuncDecl(id, p_list, e_list, t) ->
      let indent_length = indent_length + 1 in
      let tabs = String.make indent_length '\t' in
      "def " ^ id ^ " (" ^  String.concat ", " (List.map parameters_to_string p_list) ^ ") : " ^ type_to_string t ^ " =>\n" ^
      tabs ^ String.concat ("\n" ^ tabs) (List.map (expression_to_string indent_length) e_list)
  | FuncDecl(id, p_list, e_list) ->
      let indent_length = indent_length + 1 in
      let tabs = String.make indent_length '\t' in
      "def " ^ id ^ " (" ^  String.concat ", " (List.map parameters_to_string p_list) ^ ") =>\n" ^
      tabs ^ String.concat ("\n" ^ tabs) (List.map (expression_to_string indent_length) e_list)
  | TypedFuncAnon(p_list, e, t) -> 
      "(" ^  String.concat ", " (List.map parameters_to_string p_list) ^ " => " ^ 
      expression_to_string indent_length e ^ " : " ^ type_to_string t ^ ")"
  | FuncAnon(p_list, e) ->
      "(" ^  String.concat ", " (List.map parameters_to_string p_list) ^ " => " ^ 
      expression_to_string indent_length e ^ ")"
  | FuncPipe(e1, e2) ->
      "(" ^ expression_to_string indent_length e1 ^ " |> " ^ expression_to_string indent_length e2 ^ ")"
  | FuncComposition    (e1, e2) ->
      "(" ^ expression_to_string indent_length e1 ^ " >> " ^ expression_to_string indent_length e2 ^ ")"

let program_to_string expressions =
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
  | UNIT -> "UNIT"
  | CONS -> "CONS"
  | EOF -> "EOF" 
  | ID(s) -> "ID(" ^ s ^ ")"
  | INT(i) -> "INT(" ^ string_of_int i ^ ")"
  | FLOAT(f) -> "FLOAT(" ^ string_of_float f ^ ")"
  | DEDENT_COUNT(i) -> "DEDENT_COUNT(" ^ string_of_int i ^ ")"
  | BOOL(b) -> "BOOL(" ^ (if b then "true" else "false") ^ ")"
  | STRING(s) -> "STRING(" ^ s ^ ")"
  | TUPLEACC -> "TUPLEACC"
  | CHAR(c) -> "CHAR(" ^ Char.escaped c ^ ")"
  | DEDENT_EOF(i) -> "DEDENT_EOF(" ^ string_of_int i ^ ")"

let token_list_to_string token_list =
  let rec helper last_line_number = function
      (token, line)::tail ->
        (if line != last_line_number then "\n" ^ string_of_int line ^ ". " else " ") ^
        token_to_string token ^ helper line tail
    | [] -> "\n"
  in helper 0 token_list

(* Analyzer Utils *)

(* Sast Printer*)

let rec a_type_to_string = function
    Int -> "Int"
  | Unit -> "Unit"
  | Bool -> "Bool"
  | String -> "String"
  | Char -> "Char"
  | Tuple(t) -> "Tuple[" ^ String.concat ", " (List.map a_type_to_string t) ^ "]"
  | List(t) -> "List[" ^ a_type_to_string t ^ "]"
  | Float -> "Float"
  | Function(t1, t2) -> "Function(" ^ String.concat ", " (List.map a_type_to_string t1)^ " => " ^ a_type_to_string t2 ^ ")"
  | Map(t1, t2) -> "Map[" ^ a_type_to_string t1 ^ ", "^ a_type_to_string t1 ^ "]"
  | Print -> "PRINT"

let a_param_list_to_string (id, t) = id ^ ": " ^ a_type_to_string t

let rec aexpression_to_string = function
    AIntLiteral(i) -> string_of_int(i)
  | AFloatLiteral(f) -> string_of_float(f)
  | ABinop(e1, op, e2, t) ->
    "( " ^ aexpression_to_string(e1) ^ " " ^
    operation_to_string(op) ^ " " ^
    aexpression_to_string(e2) ^ " " ^
    ")" ^ "_" ^ a_type_to_string(t)
  | AUnop(op, e1, t) ->
    operation_to_string(op) ^ " " ^
    aexpression_to_string(e1) ^ "_" ^
    a_type_to_string(t)
  | ABoolLiteral(b) ->
    if b then "true"
    else "false"
  | AStringLiteral(s) -> s
  | ACharLiteral(c) -> Char.escaped c
  | AUnitLiteral -> "Unit"
  | AIdLiteral(id, t) -> id ^ "_" ^ a_type_to_string(t)
  | AAssign(id, e, t) ->
    "Assign(" ^ "_" ^ a_type_to_string t ^ ") " ^
    id ^ " = " ^
    aexpression_to_string e
  | AReassign(id, e, t) -> 
    "Reassign(" ^ "_" ^ a_type_to_string t ^ ") " ^
    id ^ " = " ^
    aexpression_to_string e
  | ATupleLiteral(e_list, t) ->
    "Tuple(" ^ String.concat ", " (List.map aexpression_to_string e_list) ^ ")" ^ "_" ^ a_type_to_string(t)
  | ATupleAccess(id, indx, t)  ->
    "TupleAccess " ^ aexpression_to_string id ^ "(" ^ aexpression_to_string indx ^ ")" ^ "_" ^ a_type_to_string(t)
  | AListLiteral(e_list, t) ->
    "List(" ^ String.concat ", " (List.map aexpression_to_string e_list) ^ ")" ^ "_" ^ a_type_to_string(t)
  | AListAccess(id, indx, t) ->
    "ListAccess " ^ " " ^ aexpression_to_string id ^ "( " ^ aexpression_to_string indx ^ ")_" ^ a_type_to_string(t)
  | AMapLiteral(map_list, t) ->
    let map_expression_tupal_to_string (e1, e2) =
      "(" ^ aexpression_to_string e1 ^ " -> " ^ aexpression_to_string e2 ^ ")"
    in
    "Map(" ^ String.concat ", " (List.map map_expression_tupal_to_string map_list) ^ ")" ^ "_" ^ a_type_to_string(t)
  | AIfBlock(e, e_list, t) ->
    "\nIf(" ^ aexpression_to_string e ^ ")\n" ^
    "\t" ^ String.concat "\n\t" (List.map aexpression_to_string e_list) ^ "_" ^ a_type_to_string(t) ^ "\n" ^
    "EndIf\n"
  | AIfElseBlock(e, e_list1, e_list2, t) ->
    "\nIf(" ^ aexpression_to_string e ^ ")\n" ^
    "\t" ^ String.concat "\n\t" (List.map aexpression_to_string e_list1) ^ "_" ^ a_type_to_string(t) ^ "\n" ^
    "Else\n" ^
    "\t" ^ String.concat "\n\t" (List.map aexpression_to_string e_list2) ^ "_" ^ a_type_to_string(t) ^ "\n" ^
    "EndIf\n"
  | AFuncDecl(id, p_list, e_list, t) ->
    if (List.length p_list) <> 0 then
      "\n def " ^ id ^ " (" ^ String.concat ", " (List.map a_param_list_to_string p_list) ^ ") : " ^ a_type_to_string t ^ " =>\n" ^
      "\t" ^ String.concat "\n\t" (List.map aexpression_to_string e_list) ^ "\n"
    else
      "\n def " ^ id ^ " : " ^ a_type_to_string t ^ " =>\n" ^
      "\t" ^ String.concat "\n\t" (List.map aexpression_to_string e_list) ^ "\n"
  | AFuncAnon(p_list, exp, t) -> 
    if (List.length p_list) <> 0 then
      "\n(" ^ String.concat ", " (List.map a_param_list_to_string (List.rev p_list)) ^ a_type_to_string t ^ " =>\n" ^
      "\t" ^ aexpression_to_string exp ^ ")\n"
    else
      "\n ( =>\n" ^
      "\t" ^ aexpression_to_string exp ^ ")\n"
  | AFuncCall(id, params, s_type) ->
    if (List.length params) <> 0 then
      "\n" ^ aexpression_to_string id ^ " (" ^ String.concat ", " (List.map aexpression_to_string params) ^ ")" ^ "_" ^ a_type_to_string(s_type) ^ "\n"
    else
      "\n " ^ aexpression_to_string id ^ "()" ^ "_" ^ a_type_to_string(s_type)
  | AFuncComposition(exp1, exp2, t) ->
      "\n" ^ aexpression_to_string exp1 ^ ">>" ^ aexpression_to_string exp2 ^ "_" ^ a_type_to_string(t) ^ "\n"
  | AMapAccess(id, param, s_type) ->
      "\nMapAccess" ^ aexpression_to_string id ^ " (" ^ aexpression_to_string param ^ ")" ^ "_" ^ a_type_to_string(s_type) ^ "\n"
  | AFuncPiping(exp1, exp2, t) ->
      "\n" ^ aexpression_to_string exp1 ^ "|>" ^ aexpression_to_string exp2 ^ "_" ^ a_type_to_string(t) ^ "\n"

let a_program_to_string a_expressions = 
  String.concat "\n" (List.map aexpression_to_string a_expressions) ^ "\n"