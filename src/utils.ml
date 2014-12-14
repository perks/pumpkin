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
  | TAlgebraic(s) -> s
  | TTuple(t)  -> "Tuple[" ^ String.concat ", " (List.map type_to_string t) ^ "]"
  | TList(t)-> "List[" ^ type_to_string t ^ "]"
  | TMap(t1, t2) -> "Map[" ^ type_to_string t1 ^ ", " ^ type_to_string t2 ^ "]"
  | TFunction(t1, t2) -> "(" ^ type_to_string t1 ^ " => " ^ type_to_string t2 ^ ")"

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
  | AlgebricAccess(e, prop) -> 
      expression_to_string indent_length e ^ "." ^ prop
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
  | MatchBlock(e, match_list) ->
      let indent_length = indent_length + 1 in
      let tabs = String.make indent_length '\t' in
      let match_expression_tupal_to_string (e1, e2) =
        expression_to_string indent_length e1 ^ " => " ^ 
        (
          match e2 with
              IfBlock(_, _)
            | IfElseBlock(_,_,_)
            | MatchBlock(_,_) ->
                let indent_length = indent_length + 1 in
                let tabs = String.make indent_length '\t' in
                "\n" ^ tabs ^ expression_to_string indent_length e2
            | _ -> expression_to_string indent_length e2
        )
      in
      "match " ^ expression_to_string indent_length e ^ " :\n" ^
      tabs ^ "| " ^ String.concat ("\n" ^ tabs ^ "| ") (List.map match_expression_tupal_to_string match_list)
  | Call(id, e_list) ->
      id ^ 
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
  | Wildcard -> "_"

let algebraic_variant_to_string = function
    VariantEmpty(id) -> id
  | VariantProduct(id, p_list) ->
      id ^ "(" ^ String.concat ", " (List.map parameters_to_string p_list) ^ ")"

let algebraic_decls_to_string = function
    AlgebraicEmpty(id) ->
      "type " ^ id
  | AlgebraicProduct(id, p_list) ->
      "type " ^ id ^ "(" ^ String.concat ", " (List.map parameters_to_string p_list) ^ ")"
  | AlgebraicSum(id, v_list) ->
      "type " ^ id ^ "=\n" ^
      "\t| " ^ String.concat "\n\t| " (List.map algebraic_variant_to_string v_list)

let program_to_string (expressions, algebraic_decls) =
  String.concat "\n"
    (
      List.append 
        (List.map algebraic_decls_to_string algebraic_decls)
        (List.map (expression_to_string 0) expressions)
    )
  ^ "\n"

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
  | TYPE -> "TYPE"
  | TMAP -> "TMAP"
  | UNIT -> "UNIT"
  | MATCH -> "MATCH" | SELECTION -> "SELECTION" | WILDCARD -> "WILDCARD"
  | CONS -> "CONS"
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
  | Algebraic(id) -> "Algebraic[" ^ id ^ "]"
  | Variant(id, t) -> "Variant[" ^ id ^ "]->" ^ a_type_to_string t
  | Float -> "Float"
  | Function(t1, t2) -> "(" ^ a_type_to_string t1 ^ " => " ^ a_type_to_string t2 ^ ")"
  | Map(t1, t2) -> "Map[" ^ a_type_to_string t1 ^ ", "^ a_type_to_string t1 ^ "]"

let a_param_list_to_string (id, t) = id ^ ": " ^ a_type_to_string t

let a_algebraic_variant_to_string = function
    AVariantEmpty(t) -> a_type_to_string t
  | AVariantProduct(t, p_list) ->
      a_type_to_string t ^ "(" ^ String.concat ", " (List.map a_param_list_to_string p_list) ^ ")"

let a_algebraic_to_string = function
    AAlgebraicEmpty(t) -> a_type_to_string t
  | AAlgebraicProduct(t, p_list) -> 
      a_type_to_string t ^ "(" ^  String.concat ", " (List.map a_param_list_to_string p_list) ^ ")"
  | AAlgebraicSum(t, v_list) ->
      a_type_to_string t  ^ "=\n" ^
      "\t| " ^ String.concat "\n\t| " (List.map a_algebraic_variant_to_string v_list)

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
  | ATupleAccess(e, e_acc, t)  ->
    "(" ^ aexpression_to_string e ^ ")TupleAccess(" ^ aexpression_to_string e_acc ^ ")" ^ "_" ^ a_type_to_string(t)
  | AListLiteral(e_list, t) ->
    "List(" ^ String.concat ", " (List.map aexpression_to_string e_list) ^ ")" ^ "_" ^ a_type_to_string(t)
  | AListAccess(e_list, i, t) ->
    "ListAccess" ^ " " ^ aexpression_to_string i ^ "_" ^ a_type_to_string(t)
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

let a_program_to_string (a_expressions, algebraic_types) = 
  String.concat "\n" (List.map a_algebraic_to_string algebraic_types) ^ "\n" ^
  String.concat "\n" (List.map aexpression_to_string a_expressions) ^ "\n"


(* 

let rec aexpression_to_string = function
  | AMapLiteral(map_list, t) ->
    let map_expression_tupal_to_string (e1, e2) =
      "(" ^ aexpression_to_string e1 ^ " -> " ^ aexpression_to_string e2 ^ ")"
    in
    "MAP(" ^ String.concat ", " (List.map map_expression_tupal_to_string map_list) ^ ")" ^ "_" ^ s_type_to_string(t)
  
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
*)