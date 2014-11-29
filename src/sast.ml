open Ast

type sTypes = Num | Unit | Bool | String

type aExpression =
    AnIntLiteral of int * sTypes
  | ABinop of aExpression * operator * aExpression * sTypes
  | ABoolLiteral of bool * sTypes
  | AStringLiteral of string * sTypes
  | AUnit of sTypes
  | ATypeAssign of string * aExpression * sTypes

type 'a aBody = aExpression list
type 'a aBlock = 'a aBody

type aRoot = aExpression aBody