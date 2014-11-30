open Ast
open Sast

let type_of = function
    AnIntLiteral(_, t) -> t
  | ABoolLiteral(_, t) -> t
  | AStringLiteral(_, t) -> t
  | ACharLiteral(_, t) -> t
  | AUnit(t) -> t
  | ABinop(_, _, _, t) -> t
  | ATypeAssign(_, _, t) -> t
  | ATupleLiteral(_, t) -> t
  | AListLiteral(_, t) -> t
  | AIfBlock(_, _, t) -> t
  | AIfElseBlock(_, _, _, t) -> t

let aType_to_saType = function
    TNum -> Num
  | TBool -> Bool
  | TString -> String
  | TChar -> Char
  | TUnit -> Unit
  | TTuple -> Tuple
  | TList -> List

let match_all_types l = 
  let temp_types = List.map type_of l in 
  let check_types rest lis = 
    if List.mem lis rest then rest 
    else lis::rest 
  in
  let unique = List.fold_left check_types [] temp_types in 
  if List.length unique != 1 then 0
  else 1

let rec annotate_expression (expr : Ast.expression) : Sast.aExpression =
  match expr with
    IntLiteral(n) -> AnIntLiteral(n, Sast.Num)
  | BoolLiteral(b) -> ABoolLiteral(b, Sast.Bool)
  | StringLiteral(s) -> AStringLiteral(s, Sast.String)
  | CharLiteral(c) -> ACharLiteral(c, Sast.Char)
  | UnitLiteral -> AUnit(Sast.Unit)
  | TupleLiteral(l) -> 
    let at_list = annotate_expression_list l in
    ATupleLiteral(at_list, Sast.Tuple)
  | ListLiteral(l) ->
    let a_list = annotate_expression_list l in
    if (match_all_types a_list) != 1 then
      raise(Failure("List elemets' types must all be the same"))
    else AListLiteral(a_list, Sast.List);
  | Binop(e1, op, e2) ->
    let ae1 = annotate_expression e1 and
        ae2 = annotate_expression e2 in
    if (match_all_types [ae1; ae2]) != 1 then
      raise (Failure ("Type Mismatch"))
    else
      let et = type_of ae1 in 
      ABinop(ae1, op, ae2, et)
  | TypeAssing(i, e, t) ->
    let ae = annotate_expression e in
    let ae_s_type = type_of ae and 
        t_s_type = aType_to_saType t in
    if ae_s_type != t_s_type then
      raise (Failure ("Type Mismatch"))
    else ATypeAssign(i, ae, t_s_type)
  | Assing(i, e) ->
    let ae = annotate_expression e in
    let ae_s_type = type_of ae in
    ATypeAssign(i, ae, ae_s_type)
  | IfBlock(e1, l) -> 
    let a_list = annotate_expression_list l in
    let ae = annotate_expression e1 and
    le = annotate_expression (List.hd (List.rev l)) in
    let ae_s_type = type_of ae and
    le_s_type = type_of le in
    if ae_s_type != Sast.Bool then
      raise (Failure ("If requires a boolean expression"))
    else AIfBlock(ae, a_list, le_s_type)
  | IfElseBlock(e1, l1, l2) ->
    let ae = annotate_expression e1 in
    let a_list1 = annotate_expression_list l1 and
    a_list2 = annotate_expression_list l2 in
    let le1 = annotate_expression (List.hd (List.rev l1)) and
    le2 = annotate_expression (List.hd (List.rev l2)) in
    let ae_s_type = type_of ae in
    let le1_s_type = type_of le1 and
    le2_s_type = type_of le2 in
    if ae_s_type != Sast.Bool then
      raise (Failure ("If requires a boolean expression"))
    else if le1_s_type != le2_s_type then
      raise (Failure ("Return type of if and else must match"))
    else AIfElseBlock(ae, a_list1, a_list2, le1_s_type)

and annotate_expression_list (expr_list : Ast.expression list) : Sast.aExpression list =
  List.map (fun expr -> annotate_expression expr) expr_list

and annotate_program (p : Ast.root) : Sast.aRoot =
  annotate_expression_list p
