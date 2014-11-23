open Ast

type sTypes = Num | Unit

type aExpression = 
    ANumLiteral of string * sTypes
  | ABinop of aExpression * operator * aExpression * sTypes
  | AUnit of sTypes

type aProgram = aExpression list