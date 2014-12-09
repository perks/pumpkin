open Ast
open Sast

module Env = Map.Make(String)

let symbolTable = Array.make 10 Env.empty
let functionsTable = ref([])

let getCurrentEnvironment = 
  let rec helper l current  =
  match l with
       [] -> current
    |  hd::tl -> if Env.is_empty hd then current else helper tl (current + 1)
  in helper (Array.to_list symbolTable) 0

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
  | AMapLiteral(_, t) -> t
  | AIfBlock(_, _, t) -> t
  | AIfElseBlock(_, _, _, t) -> t
  | AStringChars(_, t) -> t
  | AParameter(_, t) -> t
  | ATypeFuncDecl(_, _, _, t) -> t
  | AFuncCall(_, _, t) -> t
  | AFuncAnon(_, _, _, t) -> t
  | AFuncComposition(_, _, _, t) -> t
  | AFuncPiping(_, t) -> t
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

let get_function_name = function
    ATypeFuncDecl(i, _, _, _) -> i
  | AFuncCall(i, _, t) -> i
  | AFuncAnon(_, _, _, _) -> ""
  | AFuncComposition(_, _, _, _) -> ""
  | _ -> raise(Failure("Trying to get func name on non function type"))

let get_function_parameters = function
    ATypeFuncDecl(_, p, _, _) -> p
  | AFuncCall(_, p, _) -> p
  | AFuncAnon(p, _, _, _) -> p
  | AFuncComposition(p, _, _, _) -> p
  | _ -> raise(Failure("Trying to get func params on non function type"))

let get_declared_parameters = function
    AFuncDecl(_, p, _, _) -> p
  | AFuncCall(i, _, _) -> 
      let find_func_entry (a, b, c) = if (a = i && c = getCurrentEnvironment) then true else false in
      let original_function = List.find find_func_entry !functionsTable in
      (fun (a, b, c) -> b) original_function
  | AFuncAnon(p, _, _, _) -> p
  | _ -> raise(Failure("Trying to get func params on non function type"))

let get_func_return_type = function
    AFuncAnon(_, _, t, _) -> t 
  | AFuncComposition(_, _, t, _) -> t
  | AFuncCall(i, _, _) -> (Env.find i symbolTable.(getCurrentEnvironment))
  | _ -> raise(Failure("Trying to get return type on non anon func"))

let filter_params (op, gp) = 
  let rec sublist i l = 
    match l with
      [] -> []
    | h :: t -> if (i = 0) then h::t else sublist (i - 1) t     
  in let new_params = sublist (List.length gp) (List.rev op) in
  List.rev new_params

let create_new_func(i, ae, cenv) = 
  if (type_of ae) = Sast.Function then
    let original_func_name = get_function_name ae in 
    if original_func_name <> "" then
      let original_params = get_declared_parameters ae in
      let given_params = get_function_parameters ae in
      let new_params = filter_params(original_params, given_params) in
      ignore(functionsTable := (i, new_params, cenv)::!functionsTable);
      Env.find (get_function_name ae) symbolTable.(cenv)
    else 
      let params = get_function_parameters ae in
      ignore(functionsTable := (i, params, cenv)::!functionsTable);
      get_func_return_type ae
  else (type_of ae)

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
  if (type_of e1) <> (type_of e2) then
    raise (Failure ("Type Mismatch"))
  else if (op = Minus || op = Divide || op = Modulo) then
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

let valid_uniop (e, op) = 
  let t = (type_of e) in
  if (op = Minus || op = Plus) then
    if t <> Sast.Int then
      raise(Failure("Unary plus or minus requires a number"))
  else if (op = Not) then
    if t <> Sast.Bool then
      raise(Failure("Not requires a Bool type"))
  else raise(Failure("Invalid unary operator"))

let check_call (id, params, cenv) =
  let find_func_entry (a, b, c) = if (a = id && c = cenv) then true else false in
  let func_entry = List.find find_func_entry !functionsTable in
  let declared_types = (fun (a, b, c) -> b) func_entry in
  let rec match_types l1 l2 =
  match l1 with
    [] -> true
  | hd::tl -> if (type_of hd) = type_of (List.hd l2) then match_types tl (List.tl l2) else false 
  in 
  if not(match_types (List.rev params) declared_types) then
    raise(Failure("Wrong arguments/types for function call"))
  else if (List.length params) <> (List.length declared_types) then Sast.Function 
  else Env.find id symbolTable.(cenv)


let rec annotate_expression (expr : Ast.expression) : Sast.aExpression =
  match expr with
    IntLiteral(n) -> AnIntLiteral(n, Sast.Int)
  | FloatLiteral(f) -> AFloatLiteral(f, Sast.Float)
  | BoolLiteral(b) -> ABoolLiteral(b, Sast.Bool)
  | StringLiteral(s) -> AStringLiteral(s, Sast.String)
  | CharLiteral(c) -> ACharLiteral(c, Sast.Char)
  | UnitLiteral -> AUnit(Sast.Unit)
  | IdLiteral(id) -> 
    let cenv = getCurrentEnvironment in
    if Env.mem id symbolTable.(cenv) then
      let s_type = Env.find id symbolTable.(cenv) in
      AIdLiteral(id, s_type)
    else raise(Failure("Undeclared variable"))
  | TupleLiteral(l) -> 
    let at_list = annotate_expression_list l in
    ATupleLiteral(at_list, Sast.Tuple)
  | TupleAccess(e, index) -> 
    let ae = annotate_expression e and
    ind = annotate_expression index in 
    let ae_t = type_of ae and 
    ind_t = type_of ind in
    if ae_t <> Sast.Tuple then
      raise(Failure("Indexing tuple on a non tuple object"))
    else if ind_t <> Sast.Int then
      raise(Failure("Invalid type of index (expected int)"))
    else
      ATupleAccess(ae, ind, Sast.TAccess)
  | ListLiteral(l) ->
    let a_list = annotate_expression_list l in
    if not(match_all_types a_list) then
      raise(Failure("List elemets' types must all be the same"))
    else AListLiteral(a_list, Sast.List)
  | MapLiteral(elist) ->
    let s_map = List.map (fun (expr1, expr2) -> (annotate_expression expr1, annotate_expression expr2)) elist in
    let key_list = List.map (fun (expr1, expr2) -> expr1) s_map and
    value_list = List.map (fun (expr1, expr2) -> expr2) s_map in
    if not(match_all_types key_list && match_all_types value_list) then
      raise(Failure("Map element's types or key's types are not consistent"))
    else
      AMapLiteral(s_map, Sast.Map)
  | Access(e, index) -> 
    let ae = annotate_expression e and
    ind = annotate_expression index in 
    let ae_t = type_of ae in
    if ae_t <> Sast.List then
      raise(Failure("Indexing list on a non list object"))
    else
      ATupleAccess(ae, ind, Sast.LMAccess)
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
    else 
      let cenv = getCurrentEnvironment in
      let ft = create_new_func(i, ae, cenv) in
      symbolTable.(cenv) <- Env.add i ft symbolTable.(cenv);
      ATypeAssign(i, ae, ft)
  | Assing(i, e) ->
    let ae = annotate_expression e in
    let cenv = getCurrentEnvironment in
    let ft = create_new_func(i, ae, cenv) in
    symbolTable.(cenv) <- Env.add i ft symbolTable.(cenv);
    ATypeAssign(i, ae, ft)
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
    let cenv = getCurrentEnvironment in
      symbolTable.(cenv) <- Env.add s s_type symbolTable.(cenv);
      AParameter(s, s_type)
  | TypeFuncDecl(id, params, code, t) ->
    let cenv = getCurrentEnvironment in
      symbolTable.(cenv) <- Env.add id (aType_to_saType t) symbolTable.(cenv);
      symbolTable.(cenv + 1) <- symbolTable.(cenv);
    let s_params = annotate_expression_list params in 
    let s_code = annotate_expression_list code in
    let s_type = aType_to_saType t in 
    let le = annotate_expression (List.hd (List.rev code)) in 
    let le_s_type = type_of le in
    if le_s_type <> s_type then
      raise (Failure("Declared function type and actual function type don't match"))
    else 
      symbolTable.(cenv + 1) <- Env.empty;
      ignore(functionsTable := (id, s_params, cenv)::!functionsTable);
      ATypeFuncDecl(id, s_params, s_code, s_type)
  | FuncCall(id, params) -> 
    let cenv = getCurrentEnvironment in
    if Env.mem id symbolTable.(cenv) then
      let s_params = annotate_expression_list params in
      let s_type = check_call(id, s_params, cenv) in
        AFuncCall(id, s_params, s_type)
    else raise(Failure("Undeclared function"))
  | FuncAnon(params, e , t) ->
    let cenv = getCurrentEnvironment in
      symbolTable.(cenv + 1) <- symbolTable.(cenv);
    let s_params = annotate_expression_list params and 
    s_e = annotate_expression e in
    let s_e_type = type_of s_e in
    let s_type = aType_to_saType t in 
    if s_e_type <> s_type then
      raise (Failure("Declared anon function type and actual anon function type don't match"))
    else 
      symbolTable.(cenv + 1) <- Env.empty;
      AFuncAnon(s_params, [s_e], s_type, Sast.Function)
  | FuncComposition(l) ->
    let s_l =  annotate_expression_list l in
    let rec helper l current =
      match l with
        [] -> true
      | h :: t -> 
        let c_r_type = get_func_return_type current in
        let temp =  get_declared_parameters h in
        if (temp = []) then 
        (if c_r_type = Sast.Unit then helper t h else false) 
        else if c_r_type = type_of (List.hd temp) then helper t h
        else false
    in 
    if (helper (List.tl s_l) (List.hd s_l)) then 
      let new_params = get_declared_parameters (List.hd s_l) in 
      let new_return = get_func_return_type (List.hd (List.rev s_l))in
      AFuncComposition(new_params, s_l, new_return, Sast.Function)
    else raise(Failure("Composition type mismatch"))
  | FuncPiping(l) ->
    let s_l = annotate_expression_list l in
    let s_le = annotate_expression (List.hd (List.rev l)) in 
    let s_le_type = type_of s_le in
    AFuncPiping(s_l, s_le_type)
  | Block(l) ->
    let s_code = annotate_expression_list l and
    le = annotate_expression (List.hd (List.rev l)) in
    let b_type = type_of le in
    ABlock(s_code, b_type) 

and annotate_expression_list (expr_list : Ast.expression list) : Sast.aExpression list =
  List.map (fun expr -> annotate_expression expr) expr_list

and annotate_program (expressions , alg_decls) : Sast.aRoot =
  annotate_expression_list (List.rev expressions)
