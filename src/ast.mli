type operator = 
  Plus | Minus | Times | Divide | Modulo | Eq | Neq | Gt | Lt | Gte | Lte

type tType = 
	TNum | TUnit | TBool 

type expression =
    IntLiteral of int
  | BoolLiteral of bool
  | UnitLiteral
  | Binop of expression * operator * expression
  | TypeAssing of string * expression * tType

type 'a body = expression list
type 'a block = 'a body

type root = expression body