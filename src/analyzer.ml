open Sast
open Ast
open Utils

module Env = Map.Make(String)

let env_to_string id t =
  print_string(id ^ " -> " ^ a_type_to_string t ^ "\n")

let get_func_return_type = function
  Function(_, t) -> t 
  | _ -> raise(Exceptions.PipingIntoNonFunc)

let get_func_params = function
  Function(t, _) -> (List.rev t) 
  | _ -> raise(Exceptions.PipingIntoNonFunc) 

let filter_params (op, num_gp) = 
  let rec sublist i l = 
    match l with
      [] -> []
    | h :: t -> if (i = 0) then h::t else sublist (i - 1) t     
  in let new_params = sublist num_gp op in
  List.rev new_params

let rec aType_to_sType = function
    TInt -> Int
  | TFloat -> Float
  | TBool -> Bool
  | TString -> String
  | TChar -> Char
  | TUnit -> Unit
  | TTuple(t) -> Tuple((List.map aType_to_sType t))
  | TList(t) -> List(aType_to_sType t)
  | TMap(t1, t2) -> Map(aType_to_sType t1, aType_to_sType t2)
  | TFunction(t1, t2) -> Function(List.map aType_to_sType t1, aType_to_sType t2)

let rec type_of = function
    AIntLiteral(_) -> Int
  | AFloatLiteral(_) -> Float
  | ABoolLiteral(_) -> Bool
  | AStringLiteral(_) -> String
  | ACharLiteral(_) -> Char
  | AUnitLiteral -> Unit
  | ATupleLiteral(_, t) -> t
  | AListLiteral(_, t) -> t
  | AMapLiteral(_, t) -> t
  | AIdLiteral(_, t) -> t
  | ABinop(_, _, _, t) -> t
  | AUnop(_, _, t) -> t
  | AAssign(_, _, t) -> t
  | AReassign(_, _, t) -> t
  | ATupleAccess(_, _, t) -> t
  | AListAccess(_, _, t) -> t
  | AIfBlock(_, _, t) -> t
  | AIfElseBlock(_, _, _, t) -> t
  | AFuncCall(_, _, t) -> t
  | AFuncDecl(_, _, _, t) -> t
  | AFuncAnon(_, _, t) -> t
  | AFuncComposition (_, _, t) -> t
  | AFuncPiping(_, _, t) -> t
  | AMapAccess(_, _, t) -> t

(* Auxiliary functions for type checks *)
let rec evaluate_index = function
    ABinop(e1, op, e2, Int) -> 
      (match op with
        Plus -> (evaluate_index e1) + (evaluate_index e2)
      | Minus -> (evaluate_index e1) - (evaluate_index e2)
      | Times -> (evaluate_index e1) * (evaluate_index e2)
      | Divide -> (evaluate_index e1) / (evaluate_index e2)
      | Modulo -> (evaluate_index e1) mod (evaluate_index e2)
      | _ -> raise(Exceptions.InvalidIndexing("Invalid operation in index")))
  | AUnop(op, e1, Int) -> 
      (match op with
        Plus -> evaluate_index e1
      | _ -> raise(Exceptions.InvalidIndexing("Invalid operation in index")))
  | AIntLiteral(n) -> n
  | AIdLiteral(_, _) -> raise(Exceptions.InvalidIndexing("Cannot use variables for tuple index"))
  | _ -> raise(Exceptions.InvalidIndexing("Invalid expression in index"))

let check_reserved_functions (id, params, env) = 
  if (List.length params) = 1 then
  let t_p = type_of (List.hd params) in
  match t_p with
    List(t) ->
      (match id with
      AIdLiteral(i, _)->
      if i = "hd" then AFuncCall(id, params, t)
      else if i = "tl" then AFuncCall(id, params, t_p)
      else if i = "len" then AFuncCall(id, params, Int)
      else if i = "is_empty" then AFuncCall(id, params, Bool)
      else raise(Exceptions.UnimplementedCallType(1))
      | _ -> raise(Exceptions.UnimplementedCallType(2)))
  | Unit -> 
    (match id with
    AIdLiteral(i, _)->
    if i = "len" then AFuncCall(id, params, Function([List(Int)], Int))
    else if i = "is_empty" then AFuncCall(id, params, Function([List(Int)], Bool))
    else raise(Exceptions.UnimplementedCallType(1))
    | _ -> raise(Exceptions.UnimplementedCallType(2)))
  |_ -> raise(Exceptions.UnimplementedCallType(3))
  else raise(Exceptions.UnimplementedCallType(4)) 

let valid_binop (t1, t2, op) =
  if op = Cons then 
    match t2 with
    List(t) -> 
    if t = Unit then List(t1) 
    else if t<>t1 then raise(Exceptions.InvalidOperation(operation_to_string op))
    else t2    
    | _ -> raise(Exceptions.InvalidOperation(operation_to_string op))
  else
  match t1 with
  Int | Float ->
  if t1 <> t2 then raise(Exceptions.TypeMismatch)
  else if op = And || op = Or || op = Cons then 
    raise(Exceptions.InvalidOperation(operation_to_string op))
  else if op = Eq || op = Neq || op = Gt || op = Lt || op = Gte || op = Lte then Bool
  else t1
  | String | Char -> 
  if op = Plus then if t2 <> String && t2 <> Char then raise(Exceptions.TypeMismatch) else String
  else if t1 <> t2 then raise(Exceptions.TypeMismatch)
  else if op = Minus || op = Divide || op = Modulo || op = And || op = Or || op = Cons then
    raise(Exceptions.InvalidOperation(operation_to_string op))
  else if op = Eq || op = Neq || op = Gt || op = Lt || op = Gte || op = Lte then Bool
  else t1
  | Bool ->
  if t1 <> t2 then raise(Exceptions.TypeMismatch)
  else if op <> Eq && op <> Neq && op <> And && op <> Or then
    raise(Exceptions.InvalidOperation(operation_to_string op))
  else t1
  | _ -> raise(Exceptions.UnimplementedOperation(operation_to_string op, a_type_to_string t1))

let valid_unop (op, t) =
  match t with
  Int | Float ->
  if op = Not then 
    raise(Exceptions.InvalidOperation(operation_to_string op))
  | Bool ->
  if op <> Not then
    raise(Exceptions.InvalidOperation(operation_to_string op))
  | _ -> raise(Exceptions.UnimplementedOperation(operation_to_string op, a_type_to_string t))

let annotate_parameter (id, t) = (id, aType_to_sType t)

let rec match_expression_list_type = function
    fst::snd::tail ->
      if type_of fst <> type_of snd then
        false
      else
        match_expression_list_type (snd::tail)
  | _ -> true

let rec annotate_expression env = function
    IntLiteral(n) -> AIntLiteral(n), env
  | FloatLiteral(f) -> AFloatLiteral(f), env
  | BoolLiteral(b) -> ABoolLiteral(b), env
  | StringLiteral(s) -> AStringLiteral(s), env
  | CharLiteral(c) -> ACharLiteral(c), env
  | UnitLiteral -> AUnitLiteral, env
  | IdLiteral(id) ->
      if Env.mem id env then
        let t = Env.find id env in
        AIdLiteral(id, t), env
      else
        raise (Exceptions.IDNotFound id)
  | TupleLiteral(e_list) ->
    let a_e_list, env = annotate_expression_list env e_list in
    let t = List.map type_of a_e_list in
    ATupleLiteral(a_e_list, Tuple(t)), env
  | ListLiteral(e_list) ->
      let a_e_list, env = annotate_expression_list env (List.rev e_list) in
      if match_expression_list_type a_e_list then
        if List.length a_e_list = 0 then
          AListLiteral(a_e_list, List(Unit)), env
        else
          let t = type_of (List.hd a_e_list) in
          AListLiteral(a_e_list, List(t)), env
      else
        raise Exceptions.TypeMismatch
  | MapLiteral(elist) ->
    let helper(expr1, expr2) = 
      let key, env = annotate_expression env expr1 in 
      let value, env = annotate_expression env expr2 in
        (key, value)
    in
    let s_map = List.map helper (List.rev elist) in
    let key_list = List.map (fun (expr1, expr2) -> expr1) s_map and
    value_list = List.map (fun (expr1, expr2) -> expr2) s_map in
    if not(match_expression_list_type key_list && match_expression_list_type value_list) then
      raise(Exceptions.TypeMismatch)
    else
    let k_type = type_of (List.hd key_list) in 
    if(k_type = Int || k_type = String || k_type = Float || k_type = Char) then
      AMapLiteral(s_map, Map((type_of (List.hd key_list)), (type_of (List.hd value_list)))), env
    else raise(Exceptions.InvalidMapKeyType)
  | TypedAssign(id, e, t) ->
      if Env.mem id env then
        raise (Exceptions.NameCollision(id))
      else
        let a_e, env = annotate_expression env e in
        let t_a_e = type_of a_e in
        let a_t = aType_to_sType t in
        if t_a_e <> a_t then
          raise (Exceptions.TypeMismatch)
        else
          let env = Env.add id t_a_e env in
          AAssign(id, a_e, t_a_e), env
  | Assign(id, e) ->
      if Env.mem id env then
        raise (Exceptions.NameCollision(id))
      else
        let a_e, env = annotate_expression env e in
        let t_a_e = type_of a_e in
        let env = Env.add id t_a_e env in
        AAssign(id, a_e, t_a_e), env
  | Reassign(id, e) ->
    if Env.mem id env then
      let t = Env.find id env in
      let a_e, env = annotate_expression env e in 
      let t_a_e = type_of a_e in
      if t = t_a_e then AReassign(id, a_e, t), env
      else raise(Exceptions.TypeMismatch) 
    else raise(Exceptions.IDNotFound(id))
  | Binop(e1, op, e2) ->
    let ae1, env = annotate_expression env e1 in
    let ae2, env = annotate_expression env e2 in
    let t = valid_binop((type_of ae1), (type_of ae2), op) in
    ABinop(ae1, op, ae2, t), env
  | Unop(op, e) ->
    let ae, env = annotate_expression env e in
    let et = type_of ae in 
    valid_unop(op, et);
    AUnop(op, ae, et), env
  | ListAccess(e, index) -> 
    let ae, env = annotate_expression env e in
    let ind, env = annotate_expression env index in
    let ae_t = type_of ae and 
    ind_t = type_of ind in
    (
      match ae_t with
          List(t) ->
          (
            match ind_t with
                Int -> AListAccess(ae, ind, t), env
              | _ -> raise(Exceptions.InvalidIndexing(a_type_to_string ind_t))
          )
        | _ -> raise(Exceptions.InvalidIndexing(a_type_to_string ae_t))
    )
  | TupleAccess(e, index) -> 
    let ae, env = annotate_expression env e in
    let ind, env = annotate_expression env index in 
    let ae_t = type_of ae in
    (
      match ae_t with
        Sast.Tuple(t) -> 
          let ind_n = evaluate_index(ind) in 
          if ind_n >= 0 then 
            if ind_n > (List.length t) then raise(Exceptions.ArrayOutOfBounds)
            else 
            let param_type = List.nth t ind_n in
            ATupleAccess(ae, ind, param_type), env
          else raise(Exceptions.InvalidIndexing("Negative index"))
        | _ -> raise(Exceptions.InvalidIndexing(a_type_to_string ae_t))
    )
  | IfBlock(e, e_list) -> 
    let a_list, _ = annotate_expression_list env e_list in
    let ae, _ = annotate_expression env e in
    let ae_s_type = type_of ae in
    if ae_s_type = Bool then
      AIfBlock(ae, a_list, Unit), env
    else raise(Exceptions.IfRequiresBool(a_type_to_string ae_s_type))
  | IfElseBlock(e1, l1, l2) ->
    let ae, tempEnv = annotate_expression env e1 in
    let a_list1, _ = annotate_expression_list env l1 in
    let a_list2, _ = annotate_expression_list env l2 in
    let le1 = (List.hd (List.rev a_list1)) in
    let le2 = (List.hd (List.rev a_list2)) in
    let ae_s_type = type_of ae in
    let le1_s_type = type_of le1 and
    le2_s_type = type_of le2 in
    if ae_s_type <> Sast.Bool then
      raise (Exceptions.IfRequiresBool(a_type_to_string ae_s_type))
    else if le1_s_type <> le2_s_type then
      raise (Exceptions.TypeMismatch)
    else AIfElseBlock(ae, a_list1, a_list2, le1_s_type), env
  | TypedFuncDecl(id, params, code, t) ->
    if Env.mem id env then
        raise (Exceptions.NameCollision(id))
    else
    let s_params = List.map annotate_parameter (List.rev params) in
    let param_types = List.map (fun (i, t) -> t) s_params in
    let env = Env.add id (Function(param_types, (aType_to_sType t))) env in
    let tempEnv =  List.fold_left (fun cenv p -> (Env.add (fst p) (snd p) cenv)) env s_params in
    let s_code, tempEnv = annotate_expression_list tempEnv code in
    let le = (List.hd (List.rev s_code)) in
    let le_s_type = type_of le in
    let s_type = aType_to_sType t in 
    if le_s_type <> s_type && s_type <> Unit then
      raise (Exceptions.TypeMismatch)
    else 
      AFuncDecl(id, s_params, s_code, Function(param_types, s_type)), env
  | FuncDecl(id, params, code) -> 
    if Env.mem id env then
        raise (Exceptions.NameCollision(id))
    else
    let s_params = List.map annotate_parameter (List.rev params) in
    let param_types = List.map (fun (i, t) -> t) s_params in
    let tempEnv =  List.fold_left (fun cenv p -> (Env.add (fst p) (snd p) cenv)) env s_params in
    let s_code, tempEnv = annotate_expression_list tempEnv code in
    let le = (List.hd (List.rev s_code)) in
    let le_s_type = type_of le in
    let env = Env.add id (Function(param_types, le_s_type)) env in
    AFuncDecl(id, s_params, s_code, Function(param_types, le_s_type)), env
  | TypedFuncAnon(params, exp, t) ->
    let s_params = List.map annotate_parameter (List.rev params) in
    let param_types = List.map (fun (i, t) -> t) s_params in
    let tempEnv =  List.fold_left (fun cenv p -> (Env.add (fst p) (snd p) cenv)) env s_params in
    let s_e, tempEnv = annotate_expression tempEnv exp in
    let s_e_type = type_of s_e in
    let s_type = aType_to_sType t in 
    if s_e_type <> s_type && s_type <> Unit then
      raise (Exceptions.TypeMismatch)
    else 
      AFuncAnon(s_params, s_e, Function(param_types, s_type)), env
  | FuncAnon(params, exp) ->
    let s_params = List.map annotate_parameter params in
    let param_types = List.map (fun (i, t) -> t) s_params in
    let tempEnv =  List.fold_left (fun cenv p -> (Env.add (fst p) (snd p) cenv)) env s_params in
    let s_e, tempEnv = annotate_expression tempEnv exp in
    let s_e_type = type_of s_e in
    AFuncAnon(s_params, s_e, Function(param_types, s_e_type)), env
  | Call(e1, params) -> 
    let s_params, tempEnv = annotate_expression_list env (List.rev params) in
    let id, env = annotate_expression env e1 in
    let t = type_of id in
    (match t with
      Function(p, rt) ->
      if rt = Reserved then check_reserved_functions(id, s_params, env), env
      else if rt = Print then AFuncCall(id, s_params, Print), env else
      let n_params = List.length p in
      let sn_params = List.length s_params in
      let rec match_types l1 l2 =
      match l1 with
        [] -> true
      | hd::tl -> if (List.length l2 > 0 ) && (type_of hd) = (List.hd l2) then 
        match_types tl (List.tl l2) else if (type_of hd = Unit) then true else false
      in 
      let s_type = 
        if not(match_types (List.rev s_params) p) then
          raise(Exceptions.WrongParameterType(aexpression_to_string id))
        else if sn_params <> n_params then Function((filter_params (p, sn_params)), rt) 
        else rt in
      AFuncCall(id, s_params, s_type), env
    | Map(kt, vt) -> 
      if (List.length s_params) = 1 then 
        let key = List.hd s_params in
        if (kt = (type_of key)) then AMapAccess(id, key, vt), env
        else raise(Exceptions.TypeMismatch)
      else raise(Exceptions.InvalidIndexing(aexpression_to_string id))
    |  _ -> raise(Exceptions.UnimplementedCallType(111)))
  | FuncComposition(exp1, exp2) ->
    let ae1, env = annotate_expression env exp1 in
    let ae2, env = annotate_expression env exp2 in
    let t1 = type_of ae1 in 
    let t2 = type_of ae2 in
    let params = get_func_params t2 in
    if (List.length params) > 1 then raise(Exceptions.ComposedIntermediateTakesMultipleArguments)
    else
    let p_type = List.hd(params) in
    let r_type = get_func_return_type t1 in
    if(p_type <> r_type) then raise(Exceptions.TypeMismatch)
    else 
    let nr_type = get_func_return_type t2 in 
    let n_params = get_func_params t1 in
    AFuncComposition(ae1, ae2, Function(n_params, nr_type)), env
  | FuncPipe(exp1, exp2) ->
    let ae1, env = annotate_expression env exp1 in
    let ae2, env = annotate_expression env exp2 in
    let t1 = type_of ae1 in 
    let t2 = type_of ae2 in
    let params = get_func_params t2 in
    if (List.length params) > 1 then raise(Exceptions.ComposedIntermediateTakesMultipleArguments)
    else
    let p_type = List.hd(params) in
    if(p_type <> t1) then raise(Exceptions.TypeMismatch)
    else 
    let nr_type = get_func_return_type t2 in 
    AFuncPiping(ae1, ae2, nr_type), env

and annotate_expression_list env e_list =
  let env_ref = ref(env) in
  let rec helper = function
      head::tail ->
        let a_head, env = annotate_expression !env_ref head in
        env_ref := env;
        a_head::(helper tail)
    | [] -> []
  in (helper e_list), !env_ref

let annotate_program expression_list : Sast.aRoot =
  let env = Env.empty in
  let env = Env.add "print" (Function([Unit], Print)) env in
  let env = Env.add "hd" (Function([Unit], Reserved)) env in
  let env = Env.add "tl" (Function([Unit], Reserved)) env in
  let env = Env.add "len" (Function([Unit], Reserved)) env in
  let env = Env.add "is_empty" (Function([Unit], Reserved)) env in
  let a_expression_list, env = annotate_expression_list env expression_list in
  a_expression_list
