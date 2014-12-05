open Ast
open Sast

let type_of = function
    AnIntLiteral(_, t) -> t
  | AFloatLiteral(_, t) -> t
  | ABoolLiteral(_, t) -> t
  | AStringLiteral(_, t) -> t
  | ACharLiteral(_, t) -> t
  | AUnit(t) -> t
  | AIdLiteral(_, t) -> t
  | ABinop(_, _, _, t) -> t
  | AUnop(_, _, t) -> t
  | ATypeAssign(_, _, t) -> t
  | ATupleLiteral(_, t) -> t
  | ATupleAccess(_, _, t) -> t
  | AListLiteral(_, t) -> t
  | AListAccess(_, _, t) -> t
  | AIfBlock(_, _, t) -> t
  | AIfElseBlock(_, _, _, t) -> t
  | AStringChars(_, t) -> t
  | AParameter(_, t) -> t
  | AFuncDecl(_, _, _, t) -> t
  | AFuncCall(_, _, t) -> t
  | ABlock(_, t) -> t

let aType_to_saType = function
    TInt -> Int
  | TFloat -> Float
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
    if t1 <> Sast.Int || t1 <> Sast.Float then
      raise(Failure("Operator requires a number"))
  else if op = Gt || op = Lt ||op = Gte ||op = Lte || op = Plus ||op = Times then
    if t1 <> Sast.Int && t1 <> Sast.Float && t1 <> Sast.String && t1 <> Sast.Tuple && t1 <> Sast.List then
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
  | FloatLiteral(f) -> AFloatLiteral(f, Sast.Float)
  | BoolLiteral(b) -> ABoolLiteral(b, Sast.Bool)
  | StringLiteral(s) -> AStringLiteral(s, Sast.String)
  | CharLiteral(c) -> ACharLiteral(c, Sast.Char)
  | UnitLiteral -> AUnit(Sast.Unit)
  | IdLiteral(id) -> AIdLiteral(id, Sast.Id)
  | TupleLiteral(l) -> 
    let at_list = annotate_expression_list l in
    ATupleLiteral(at_list, Sast.Tuple)
  | TupleAccess(e, index) -> 
    let ae = annotate_expression e in 
    let ae_t = type_of ae in
    if ae_t <> Sast.Id && ae_t <> Sast.Tuple then
      raise(Failure("Indexing tuple on a non tuple object"))
    else
      ATupleAccess(ae, index, Sast.TAccess)
  | ListLiteral(l) ->
    let a_list = annotate_expression_list l in
    if not(match_all_types a_list) then
      raise(Failure("List elemets' types must all be the same"))
    else AListLiteral(a_list, Sast.List)
  | ListAccess(e, index) -> 
    let ae = annotate_expression e in 
    let ae_t = type_of ae in
    if ae_t <> Sast.Id && ae_t <> Sast.List then
      raise(Failure("Indexing list on a non list object"))
    else
      ATupleAccess(ae, index, Sast.LAccess)
  | Binop(e1, op, e2) ->
    let ae1 = annotate_expression e1 and
        ae2 = annotate_expression e2 in
    valid_binop (ae1, ae2, op);
    let et = type_of ae1 in 
    ABinop(ae1, op, ae2, et)
  | Unop(op, e) ->
    let ae = annotate_expression e in
    let et = type_of ae in 
    valid_uniop(ae, op);
    AUnop(op, ae, et)
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
  | Parameter(s, t) -> 
    let s_type = aType_to_saType t in
    AParameter(s, s_type)
  | FuncDecl(s, params, code, t) ->
    let s_params = annotate_expression_list params in 
    let s_code = annotate_expression_list code in
    let s_type = aType_to_saType t in 
    let le = annotate_expression (List.hd (List.rev code)) in 
    let le_s_type = type_of le in
    if le_s_type <> s_type then
      raise (Failure("Declared function type and actual function type don't match"))
    else AFuncDecl(s, s_params, s_code, s_type)
  | FuncCall(id, params) -> 
    let s_params = annotate_expression_list params in 
    AFuncCall(id, s_params, Sast.Function)
  | Block(l) ->
    let s_code = annotate_expression_list l and
    le = annotate_expression (List.hd (List.rev l)) in
    let b_type = type_of le in
    ABlock(s_code, b_type) 

and annotate_expression_list (expr_list : Ast.expression list) : Sast.aExpression list =
  List.map (fun expr -> annotate_expression expr) expr_list

and annotate_program (p : Ast.root) : Sast.aRoot =
  annotate_expression_list p
