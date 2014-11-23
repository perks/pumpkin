open Ast

type sTypes = Num | Unit | Bool

type aExpression =
    AnIntLiteral of string * sTypes
  | ABinop of aExpression * operator * aExpression * sTypes
  | ABoolLiteral of bool * sTypes
  | AUnit of sTypes
  | ATypeAssign of string * string * sTypes

type aProgram = aExpression list