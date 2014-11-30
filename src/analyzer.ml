open Ast
open Sast

let type_of = function
    AnIntLiteral(_, t) -> t
  | ABoolLiteral(_, t) -> t
  | AStringLiteral(_, t) -> t
  | ACharLiteral(_, t) -> t
  | AUnit(t) -> t
  | ABinop(_, _, _, t) -> t
  | AUniop(_, _, t) -> t
  | ATypeAssign(_, _, t) -> t
  | ATupleLiteral(_, t) -> t
  | AListLiteral(_, t) -> t
  | AIfBlock(_, _, t) -> t
  | AIfElseBlock(_, _, _, t) -> t
  | AStringChars(_, t) -> t
  | AStringInterpolation(_, t) -> t

let aType_to_saType = function
    TInt -> Int
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
  if List.length unique <> 1 then false
  else true

let valid_binop (e1, e2, op) = 
  let t1 = (type_of e1) in
  if (op = Minus || op = Divide || op = Modulo) then
    if t1 <> Sast.Int then
      raise(Failure("Operator requires a number"))
  else if op = Gt || op = Lt ||op = Gte ||op = Lte || op = Plus ||op = Times then
    if t1 <> Sast.Int && t1 <> Sast.String && t1 <> Sast.Tuple && t1 <> Sast.List then
      raise (Failure ("Invalid operator for selected types"))
  else if op = And || op = Or then
    if (type_of e1) <> Sast.Bool then
      raise (Failure ("Operator requires Bool type"))
  else if op = Not then
    raise(Failure("Invalid Binop"))
  else if (type_of e1) <> (type_of e2) then
    raise (Failure ("Type Mismatch"))

let valid_uniop (e, op) = 
  let t = (type_of e) in
  if (op = Minus || op = Plus) then
    if t <> Sast.Int then
      raise(Failure("Unary plus or minus requires a number"))
  else if (op = Not) then
    if t <> Sast.Bool then
      raise(Failure("Not requires a Bool type"))
  else raise(Failure("Invalid unary operator"))


let rec annotate_expression (expr : Ast.expression) : Sast.aExpression =
  match expr with
    IntLiteral(n) -> AnIntLiteral(n, Sast.Int)
  | BoolLiteral(b) -> ABoolLiteral(b, Sast.Bool)
  | StringLiteral(s) -> AStringLiteral(s, Sast.String)
  | CharLiteral(c) -> ACharLiteral(c, Sast.Char)
  | UnitLiteral -> AUnit(Sast.Unit)
  | TupleLiteral(l) -> 
    let at_list = annotate_expression_list l in
    ATupleLiteral(at_list, Sast.Tuple)
  | ListLiteral(l) ->
    let a_list = annotate_expression_list l in
    if not(match_all_types a_list) then
      raise(Failure("List elemets' types must all be the same"))
    else AListLiteral(a_list, Sast.List);
  | Binop(e1, op, e2) ->
    let ae1 = annotate_expression e1 and
        ae2 = annotate_expression e2 in
    valid_binop (ae1, ae2, op);
    let et = type_of ae1 in 
    ABinop(ae1, op, ae2, et)
  | Uniop(op, e) ->
    let ae = annotate_expression e in
    let et = type_of ae in 
    valid_uniop(ae, op);
    AUniop(op, ae, et)
  | TypeAssing(i, e, t) ->
    let ae = annotate_expression e in
    let ae_s_type = type_of ae and 
        t_s_type = aType_to_saType t in
    if ae_s_type <> t_s_type then
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
    if ae_s_type <> Sast.Bool then
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
    if ae_s_type <> Sast.Bool then
      raise (Failure ("If requires a boolean expression"))
    else if le1_s_type <> le2_s_type then
      raise (Failure ("Return type of if and else must match"))
    else AIfElseBlock(ae, a_list1, a_list2, le1_s_type)
  | StringChars(s) -> AStringChars(s, Sast.String)
  | StringInterpolation(l) ->
    let a_list = annotate_expression_list l in
    AStringInterpolation(a_list, Sast.String)

and annotate_expression_list (expr_list : Ast.expression list) : Sast.aExpression list =
  List.map (fun expr -> annotate_expression expr) expr_list

and annotate_program (p : Ast.root) : Sast.aRoot =
  annotate_expression_list p
