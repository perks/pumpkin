type operator = 
  Plus | Minus | Times | Divide | Modulo | Eq | Neq | Gt | Lt | Gte | Lte | And | Or | Not

type tType = 
	TInt | TUnit | TBool | TString | TChar | TTuple | TList

type expression =
    IntLiteral of int
  | BoolLiteral of bool
  | StringLiteral of string
  | CharLiteral of char
  | UnitLiteral
  | Binop of expression * operator * expression
  | Uniop of operator * expression
  | TypeAssing of string * expression * tType
  | Assing of string * expression
  | TupleLiteral of expression list
  | ListLiteral of expression list
  | Block of expression list
  | IfBlock of expression * expression list
  | IfElseBlock of expression * expression list * expression list
  | StringChars of string
  | StringInterpolation of expression list

type root = expression list