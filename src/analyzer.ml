open Ast
open Sast

let type_of = function
    AnIntLiteral(_, t) -> t
  | ABinop(_, _, _, t) -> t
  | AUnit(t) -> t

let rec annotate_expression (expr : Ast.expression) : Sast.aExpression =
  match expr with
    IntLiteral(n) -> AIntLiteral(n, Sast.Num)
  | Unit -> AUnit(Sast.Unit)
  | Binop(e1, op, e2) ->
    let ae1 = annotate_expression e1 and
        ae2 = annotate_expression e2 in
    let ae1_s_type = type_of ae1 and
        ae2_s_type = type_of ae2 in
    if ae1_s_type != ae2_s_type then
      raise (Failure ("Type Mismatch"))
    else ABinop(ae1, op, ae2, ae1_s_type)

and annotate_expression_list (expr_list : Ast.expression list) : Sast.aExpression list =
  List.map (fun expr -> annotate_expression expr) expr_list

and annotate_program (p : Ast.program) : Sast.aProgram =
  annotate_expression_list p
