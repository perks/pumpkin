open Ast

type sTypes = 
    Int 
  | Unit 
  | Bool 
  | String 
  | Char 
  | Tuple of sTypes list
  | List of sTypes
  | Algebraic of string
  | Variant of string * sTypes
  | Float 
  | Function of sTypes list * sTypes
  | Map of sTypes * sTypes
  | Print
  
and aParameter = string * sTypes

and aVariant = 
    AVariantEmpty of sTypes
  | AVariantProduct of sTypes * aParameter list

and aAlgebraic = 
    AAlgebraicEmpty of sTypes
  | AAlgebraicProduct of sTypes * aParameter list
  | AAlgebraicSum of sTypes * aVariant list

and aExpression =
    AIntLiteral of int
  | AFloatLiteral of float
  | ABoolLiteral of bool
  | AStringLiteral of string
  | ACharLiteral of char
  | AUnitLiteral
  | ATupleLiteral of aExpression list * sTypes
  | AListLiteral of aExpression list * sTypes
  | AMapLiteral of (aExpression * aExpression) list * sTypes
  | AWildcard of sTypes
  | AIdLiteral of string * sTypes
  | ABinop of aExpression * operator * aExpression * sTypes
  | AUnop of operator * aExpression * sTypes
  | AAssign of string * aExpression * sTypes
  | AReassign of string * aExpression * sTypes
  | ATupleAccess of aExpression * aExpression * sTypes
  | AListAccess of aExpression * aExpression * sTypes
  | AMapAccess of aExpression * aExpression * sTypes
  | AAlgebricAccess of aExpression * string * sTypes
  | AIfBlock of aExpression * aExpression list * sTypes
  | AIfElseBlock of aExpression * aExpression list * aExpression list * sTypes
  | AMatchBlock of aExpression * (aExpression * aExpression) list * sTypes
  | AFuncCall of aExpression * (aExpression list) * sTypes
  | AFuncDecl of string * aParameter list * aExpression list * sTypes
  | AFuncAnon of aParameter list * aExpression * sTypes
  | AFuncComposition of aExpression * aExpression * sTypes
  | AFuncPiping of aExpression * aExpression * sTypes

and aRoot = aExpression list * aAlgebraic list
