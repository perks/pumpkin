type operator = 
  Plus | Minus | Times | Divide | Modulo | Eq | Neq | Gt | Lt | Gte | Lte

type tType = 
	TNum | TUnit | TBool 

type expression =
    IntLiteral of int
  | BoolLiteral of bool
  | Unit
  | Binop of expression * operator * expression
  | TypeAssign of string * tType * expression

type 'a body = expression list
type 'a block = 'a body

type root = expression body