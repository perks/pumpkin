open Ast

type sTypes = Num | Unit | Bool | String | Char | Tuple | List

type aExpression =
    AnIntLiteral of int * sTypes
  | ABinop of aExpression * operator * aExpression * sTypes
  | ABoolLiteral of bool * sTypes
  | AStringLiteral of string * sTypes
  | ACharLiteral of string * sTypes
  | AUnit of sTypes
  | ATypeAssign of string * aExpression * sTypes
  | ATupleLiteral of aExpression list * sTypes
  | AListLiteral of aExpression list * sTypes

type 'a aBody = aExpression list
type 'a aBlock = 'a aBody

type aRoot = aExpression aBody