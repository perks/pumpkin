open Ast
open Sast

let operation_to_string = function
    Plus -> " + "
  | Minus -> " - "
  | Times -> " * "
  | Divide -> " / "
  | Modulo -> " % "
  | Eq -> " === "
  | Neq -> " !== "
  | Gt -> " > "
  | Lt -> " < "
  | Gte -> " >= "
  | Lte -> " <= "
  | And -> " && "
  | Or -> " || "
  | Not -> "!"


let rec expression_to_string = function
    AIntLiteral(i, _) -> string_of_int(i)
  | AFloatLiteral(f, _) -> string_of_float(f)
  | ABoolLiteral(b, _) -> if b then "true" else "false"
  | AStringLiteral(s, _) -> s
  | ACharLiteral(c, _) -> Char.escaped c
  | AIdLiteral(id, _) -> id
  | ABinop(e1, op, e2, _) ->
      expression_to_string e1 ^ " " ^
      operation_to_string op ^ " " ^
      expression_to_string e2 ^ ";"
  | ATypeAssign(id, e, t) ->
      "var " ^ id ^
      " = " ^
      expression_to_string e ^ ";"
  | AIfBlock(e, e_list, _) ->
      "\nif(" ^ expression_to_string e ^ ") {" ^
      "\t" ^ String.concat "\n\t" (List.map expression_to_string e_list) ^
      "\n}\n"
  | AParameter(id, t) -> id
  | ATypeFuncDecl(id, p_list, e_list, _) ->
      if (List.length p_list) <> 0 then
        "\nfunction " ^ id ^ "(" ^  
        String.concat ", " (List.map expression_to_string (List.rev p_list)) ^ ")" ^
        " \n{\n" ^ String.concat "\n\t" (List.map expression_to_string e_list) ^
        "\n};\n"
     else 
       "\nfunction " ^ id ^ "() {" ^
       "\n\t" ^ String.concat "\n\t" (List.map expression_to_string e_list) ^
       "\n};\n"
  | AFuncCall(id, p_list, _) ->
      if (List.length p_list) <> 0 then
        id ^ "(" ^ 
        String.concat ", " (List.map expression_to_string (List.rev p_list)) ^ 
        ");"
      else
        id ^ "();"


let gen_program (root : Sast.aExpression list) = 
  String.concat "\n" (List.map expression_to_string root)
