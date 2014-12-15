open Ast
open Sast

let rec strip_semicolon l =
  match l with 
      [] -> ' '::[]
    | (hd::tl) -> if (hd = ';') then strip_semicolon tl else hd::(strip_semicolon tl)

let explode s =
  let rec exp i l =
    if i < 0 then l 
    else exp(i - 1)(s.[i] :: l) in
      exp(String.length s - 1)[]

let implode l =
  let res = String.create(List.length l) in
  let rec imp i = function
      [] -> res
    | c :: l -> res.[i] <-c; imp(i + 1) l in
      imp 0 l


let flip_last = fun l ->
  let r = List.rev l
  in (List.hd r) :: (List.rev (List.tl r))

let sanitize str =
  implode ( strip_semicolon ( explode ( str) ) )

let operation_to_string = function
    Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Modulo -> "%"
  | Eq -> "==="
  | Neq -> "!=="
  | Gt -> ">"
  | Lt -> "<"
  | Gte -> ">="
  | Lte -> "<="
  | And -> "&&"
  | Or -> "||"
  | Not -> "!"

let param_list_to_js (id, t) = id

let rec aexpression_to_js = function
    AIntLiteral(i) -> string_of_int(i)
  | AFloatLiteral(f) -> string_of_float(f)
  | ABinop(e1, op, e2, t) ->
      if op = Cons then
        "cons(" ^ 
        sanitize(aexpression_to_js e1) ^ "," ^
        sanitize(aexpression_to_js e2) ^ ");"
      else
        sanitize(aexpression_to_js e1) ^ " " ^
        operation_to_string op ^ " " ^
        sanitize(aexpression_to_js e2) ^ ";"
  | AUnop(op, e1, t) ->
     "(" ^  operation_to_string(op) ^ " " ^
      aexpression_to_js(e1) ^ ")"
      (* Unops won't be bythemselves as a single expression
       * so no semicolon *)
  | ABoolLiteral(b) ->
      if b then "true"
      else "false"
  | AStringLiteral(s) -> s
  | ACharLiteral(c) -> "'" ^ Char.escaped c ^ "'"
  | AUnitLiteral -> "void"
  | AIdLiteral(id, t) -> id
  | AAssign(id, e, t) ->
      "var" ^ " " ^ id ^ " = " ^
      aexpression_to_js e ^ ";"
  | AReassign(id, e, t) ->
      id ^ " = " ^ aexpression_to_js e ^ ";"
  | AMapLiteral(map_list, t) ->
      let map_expression_tupal_to_string (e1, e2) =
        (aexpression_to_js e1) ^ ": " ^ (aexpression_to_js e2)
      in "{" ^ String.concat ", " (List.map map_expression_tupal_to_string
      map_list) ^ "};"

  | AListLiteral(e_list, t) ->
      "[" ^ String.concat ", " (List.map aexpression_to_js e_list) ^ "];"
  | AListAccess(id, indx, t) ->
      sanitize(aexpression_to_js id) ^ "[" ^ sanitize(aexpression_to_js indx) ^ "];"
  | AIfBlock(e, e_list, _) ->
      "\nif(" ^ sanitize(aexpression_to_js e) ^ ") {" ^
      "\n" ^ String.concat "\n\t" (List.map aexpression_to_js e_list) ^
      "\n}\n"
  | AIfElseBlock(e, e_list1, e_list2, _) -> 
      "\nif(" ^ sanitize(aexpression_to_js e) ^ ") {" ^
      "\n\t" ^ String.concat "\n\t" (List.map aexpression_to_js e_list1) ^
      "\n}\n" ^
      "else {" ^
      "\n\t" ^ String.concat "\n\t" (List.map aexpression_to_js e_list2) ^
      "\n}\n"
  | AFuncDecl(id, p_list, e_list, t) ->
      if t = Unit then
        if (List.length p_list) <> 0 then
          "function " ^ id ^ "(" ^  
          String.concat ", " (List.map param_list_to_js (List.rev p_list)) ^ ")" ^
          " \n{\n" ^ String.concat "\n\t" (List.map aexpression_to_js e_list) ^
          "\n};\n"
        else 
         "function " ^ id ^ "() {" ^
         "\n" ^ String.concat "\n\t" (List.map aexpression_to_js e_list) ^
         "\n};\n"
      else 
        if (List.length p_list) <> 0 then
          "function " ^ id ^ "(" ^
          String.concat ", " (List.map param_list_to_js (List.rev p_list)) ^ ")" ^
          "\n{\n\t" ^ String.concat "\n\t" (List.map aexpression_to_js
          (List.tl (flip_last e_list))) ^
          "\n\treturn " ^ aexpression_to_js (List.hd (flip_last e_list)) ^
          "\n};\n"
        else
          "function " ^ id ^ "() {" ^
          "\n\t" ^ String.concat "\n\t" (List.map aexpression_to_js (List.tl (flip_last e_list))) ^
          "\n\t\treturn " ^ aexpression_to_js (List.hd (flip_last e_list)) ^
          "\n};\n"
  | AFuncAnon(p_list, exp, t) ->
      if t = Unit then 
        if (List.length p_list) <> 0 then
          "function(" ^  
          String.concat ", " (List.map param_list_to_js (List.rev p_list)) ^ ")" ^
          " \n{\n\t" ^ aexpression_to_js exp ^
          "\n};\n"
        else 
         "\nfunction() {" ^
         "\n\t" ^ aexpression_to_js exp ^
         "\n};\n"
      else
        if (List.length p_list) <> 0 then
          "\nfunction(" ^
          String.concat ", " (List.map param_list_to_js (List.rev p_list)) ^ ")" ^
          "\n{\n\treturn " ^ aexpression_to_js exp ^
          "\n};\n"
        else
          "function() {" ^
          "\n\treturn " ^ aexpression_to_js exp ^
          "\n};\n"
  | ACall(id, params, s_type) ->
      if (List.length params) <> 0 then
        " " ^ id ^ "(" ^ String.concat ", " (List.map aexpression_to_js params) ^
        ")"
        else
          " " ^ id ^ "()"

let pumpkin_to_js (a_expressions, algebraic_types) =
  String.concat "\n" (List.map aexpression_to_js a_expressions)

(*let rec expression_to_string = function*)
    (*AIntLiteral(i, _) -> string_of_int(i)*)
  (*| AFloatLiteral(f, _) -> string_of_float(f)*)
  (*| ABoolLiteral(b, _) -> if b then "true" else "false"*)
  (*| AStringLiteral(s, _) -> s*)
  (*| ACharLiteral(c, _) -> Char.escaped c*)
  (*| AIdLiteral(id, _) -> id*)
  (*| ABinop(e1, op, e2, _) ->*)
      (*expression_to_string e1 ^ " " ^*)
      (*operation_to_string op ^ " " ^*)
      (*expression_to_string e2 ^ ";"*)
  (*| ATypeAssign(id, e, t) ->*)
      (*"var " ^ id ^*)
      (*" = " ^*)
      (*expression_to_string e ^ ";"*)
  (*| AParameter(id, t) -> id*)
  (*| ATypeFuncDecl(id, p_list, e_list, t) ->*)

  (*| AFuncCall(id, p_list, _) ->*)
      (*if (List.length p_list) <> 0 then*)
        (*id ^ "(" ^ *)
        (*implode(strip_semicolon (explode(String.concat ", " (List.map expression_to_string (List.rev p_list))))) ^ *)
        (*");"*)
      (*else*)
        (*id ^ "();"*)



(*let gen_program (root : Sast.aExpression list) = *)
  (*String.concat "\n" (List.map expression_to_string root)*)
