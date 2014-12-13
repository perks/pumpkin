open Sast
open Ast
open Utils

module Env = Map.Make(String)

let type_env_ref = ref(Env.empty)

let env_to_string id t =
  print_string(id ^ " -> " ^ a_type_to_string t ^ "\n")

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
  | TFunction(t1, t2) -> Function(aType_to_sType t1, aType_to_sType t2)
  | TAlgebraic(id) ->
      let type_env = !type_env_ref in
      if Env.mem id type_env then
        Env.find id type_env
      else
        raise (Exceptions.TypeNotFound(id))

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
  | AWildcard(t) -> t
  | AIdLiteral(_, t) -> t
  | ABinop(_, _, _, t) -> t
  | AUnop(_, _, t) -> t
  | AAssign(_, _, t) -> t
  | AReassign(_, _, t) -> t
  | ATupleAccess(_, _, t) -> t
  | AListAccess(_, _, t) -> t
  | AAlgebricAccess(_, _, t) -> t
  | AIfBlock(_, _, t) -> t
  | AIfElseBlock(_, _, _, t) -> t
  | AMatchBlock(_, _, t) -> t
  | ACall(_, _, t) -> t
  | AFuncDecl(_, _, _, t) -> t
  | AFuncAnon(_, _, t) -> t
  | AFuncComposition (_, _, t) -> t
  | AFuncPiping(_, _, t) -> t

(* Auxiliary functions for type checks *)

let valid_binop (t1, t2, op) =
  if op = Cons then 
    match t2 with
    List(t) -> 
    if t = Unit then List(t1) 
    else if t<>t1 then raise(Exceptions.InvalidOperation(operation_to_string op))
    else t    
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

let annotate_variant_types base_type variant =
  let add_record id t =
    let type_env = !type_env_ref in
    if Env.mem id type_env then
      raise (Exceptions.NameCollision(id))
    else
      type_env_ref := (Env.add id t type_env)
  in
  match variant with
      VariantEmpty(id) -> 
        let t = Variant(id, base_type) in
        add_record id t;
        AVariantEmpty(t)
    | VariantProduct(id, p_list) ->
        let t = Variant(id, base_type) in
        add_record id t;
        AVariantProduct(t, List.map annotate_parameter p_list)

let annotate_algebraic_types alg_decl = 
  let add_record id t =
    let type_env = !type_env_ref in
    if Env.mem id type_env then
      raise (Exceptions.NameCollision(id))
    else
      type_env_ref := (Env.add id t type_env);
  in
  match alg_decl with
    AlgebraicEmpty(id) ->
      let t = Algebraic(id) in
      add_record id t;
      AAlgebraicEmpty(t)
  | AlgebraicProduct(id, p_list) -> 
      let t = Algebraic(id) in
      add_record id t;
      AAlgebraicProduct(t, List.map annotate_parameter p_list)
  | AlgebraicSum(id, v_list) -> 
      let t = Algebraic(id) in
      add_record id t;
      AAlgebraicSum(t, List.map (annotate_variant_types t) v_list)

let rec match_expression_list_type = function (* does not deal with algebraic properly *)
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
      let a_e_list, env = annotate_expression_list env e_list in
      if match_expression_list_type a_e_list then
        if List.length a_e_list = 0 then
          AListLiteral(a_e_list, List(Unit)), env
        else
          let t = type_of (List.hd a_e_list) in
          AListLiteral(a_e_list, List(t)), env
      else
        raise Exceptions.TypeMismatch
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
    let ind_t = type_of ind in
    if ind_t <> Int then raise(Exceptions.TypeMismatch) else
    let ae_t = type_of ae in
    (
      match ae_t with
          Sast.List(t) -> AListAccess(ae, ind, t), env
        | _ -> raise(Failure("Indexing list on a non list object"))
    )


(*
  | Reassign of string * expression
  | AlgebricAccess of expression * string
  | IfBlock of expression * expression list
  | IfElseBlock of expression * expression list * expression list
  | MatchBlock of expression * (expression * expression) list
  | Call of string * (expression list)
  | TypedFuncDecl of string * parameter list * expression list * tTypes
  | FuncDecl of string * parameter list * expression list
  | TypedFuncAnon of parameter list * expression * tTypes
  | FuncAnon of parameter list * expression
  | FuncPipe of expression * expression
  | FuncComposition of expression * expression
  
  
  | AMapLiteral of (aExpression * aExpression) list * sTypes
  | AWildcard
  | AReassign of string * aExpression * sTypes
  | ATupleAccess of aExpression * aExpression * sTypes
  | AListAccess of aExpression * aExpression * sTypes
  | AAlgebricAccess of aExpression * string * sTypes
  | AIfBlock of aExpression * aExpression list * sTypes
  | AIfElseBlock of aExpression * aExpression list * aExpression list * sTypes
  | AMatchBlock of aExpression * (aExpression * aExpression) list * sTypes
  | ACall of string * (aExpression list) * sTypes
  | AFuncDecl of string * aParameter list * aExpression list * sTypes
  | AFuncAnon of aParameter list * aParameter list * sTypes
  | AFuncComposition of aExpression * aExpression * sTypes
  | AFuncPiping of aExpression * aExpression * sTypes
*)

and annotate_expression_list env e_list =
  let env_ref = ref(env) in
  let rec helper = function
      head::tail ->
        let a_head, env = annotate_expression !env_ref head in
        env_ref := env;
        a_head::(helper tail)
    | [] -> []
  in (helper e_list), !env_ref

let annotate_program (expression_list, alg_decl_list) : Sast.aRoot =
  let a_alg_structures = List.map annotate_algebraic_types alg_decl_list in
  let env = Env.empty in
  let a_expression_list, env = annotate_expression_list env expression_list in
  Env.iter env_to_string env;
  a_expression_list, a_alg_structures