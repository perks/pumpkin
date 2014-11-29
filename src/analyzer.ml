open Ast
open Sast

let type_of = function
    AnIntLiteral(_, t) -> t
  | ABoolLiteral(_, t) -> t
  | AUnit(t) -> t
  | ABinop(_, _, _, t) -> t
  | ATypeAssign(_, _, t) -> t

let aType_to_saType = function
    TNum -> Num
  | TUnit -> Unit
  | TBool -> Bool

let rec annotate_expression (expr : Ast.expression) : Sast.aExpression =
  match expr with
    IntLiteral(n) -> AnIntLiteral(n, Sast.Num)
  | BoolLiteral(b) -> ABoolLiteral(b, Sast.Bool)
  | UnitLiteral -> AUnit(Sast.Unit)
  | Binop(e1, op, e2) ->
    let ae1 = annotate_expression e1 and
        ae2 = annotate_expression e2 in
    let ae1_s_type = type_of ae1 and
        ae2_s_type = type_of ae2 in
    if ae1_s_type != ae2_s_type then
      raise (Failure ("Type Mismatch"))
    else ABinop(ae1, op, ae2, ae1_s_type)
  | TypeAssing(i, e, t) ->
    let ae = annotate_expression e in
    let ae_s_type = type_of ae and 
        t_s_type = aType_to_saType t in
    if ae_s_type != t_s_type then
      raise (Failure ("Type Mismatch"))
    else ATypeAssign(i, ae, t_s_type)


and annotate_expression_list (expr_list : Ast.expression list) : Sast.aExpression list =
  List.map (fun expr -> annotate_expression expr) expr_list

and annotate_program (p : Ast.root) : Sast.aRoot =
  annotate_expression_list p
