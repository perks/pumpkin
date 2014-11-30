type operator = 
  Plus | Minus | Times | Divide | Modulo | Eq | Neq | Gt | Lt | Gte | Lte | And | Or | Not

type tType = 
	TNum | TUnit | TBool | TString | TChar | TTuple | TList

type expression =
    IntLiteral of int
  | BoolLiteral of bool
  | StringLiteral of string
  | CharLiteral of char
  | UnitLiteral
  | Binop of expression * operator * expression
  | Uniop of operator * expression
  | TypeAssing of string * expression * tType
  
type root = expression list