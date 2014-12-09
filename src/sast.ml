open Ast

type sTypes = Int | Unit | Bool | String | Char | Tuple | List | Float | Function | TAccess | LMAccess | Map

type aExpression =
    AnIntLiteral of int * sTypes
  | AFloatLiteral of float * sTypes
  | ABinop of aExpression * operator * aExpression * sTypes
  | AUnop of operator * aExpression * sTypes
  | ABoolLiteral of bool * sTypes
  | AStringLiteral of string * sTypes
  | ACharLiteral of char * sTypes
  | AUnit of sTypes
  | AIdLiteral of string * sTypes
  | ATypeAssign of string * aExpression * sTypes
  | ATupleLiteral of aExpression list * sTypes
  | ATupleAccess of aExpression * aExpression * sTypes
  | AListLiteral of aExpression list * sTypes
  | AListAccess of aExpression * int * sTypes
  | AMapLiteral of (aExpression * aExpression) list * sTypes
  | AIfBlock of aExpression * aExpression list * sTypes
  | AIfElseBlock of aExpression * aExpression list * aExpression list * sTypes
  | AStringChars of string * sTypes
  | AParameter of string * sTypes
  | ATypeFuncDecl of string * aExpression list * aExpression list * sTypes
  | AFuncCall of string * aExpression list * sTypes
  | AFuncAnon of aExpression list * aExpression list * sTypes * sTypes
  | AFuncComposition of aExpression list * aExpression list * sTypes *sTypes
  | AFuncPiping of aExpression list * sTypes
  | ABlock of aExpression list * sTypes

type aRoot = aExpression list
