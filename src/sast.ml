open Ast

type sTypes = Int | Unit | Bool | String | Char | Tuple | List | Float

type aExpression =
    AnIntLiteral of int * sTypes
  | AFloatLiteral of float * sTypes
  | ABinop of aExpression * operator * aExpression * sTypes
  | AUnop of operator * aExpression * sTypes
  | ABoolLiteral of bool * sTypes
  | AStringLiteral of string * sTypes
  | ACharLiteral of char * sTypes
  | AUnit of sTypes
  | ATypeAssign of string * aExpression * sTypes
  | ATupleLiteral of aExpression list * sTypes
  | AListLiteral of aExpression list * sTypes
  | AIfBlock of aExpression * aExpression list * sTypes
  | AIfElseBlock of aExpression * aExpression list * aExpression list * sTypes
  | AStringChars of string * sTypes
  | AParameter of string * sTypes
  | AFuncDecl of string * aExpression list * aExpression list * sTypes 


type aRoot = aExpression list
