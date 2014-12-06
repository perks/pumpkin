type operator = 
  Plus | Minus | Times | Divide | Modulo | Eq | Neq | Gt | Lt | Gte | Lte | And | Or | Not

type tTypes = 
	TInt | TUnit | TBool | TString | TChar | TTuple | TList | TFloat

type expression =
    IntLiteral of int
  | FloatLiteral of float
  | BoolLiteral of bool
  | StringLiteral of string
  | CharLiteral of char
  | UnitLiteral
  | IdLiteral of string
  | Binop of expression * operator * expression
  | Unop of operator * expression
  | TypeAssing of string * expression * tTypes
  | Assing of string * expression
  | TupleLiteral of expression list
  | TupleAccess of expression * expression
  | ListLiteral of expression list
  | MapLiteral of (expression * expression) list
  | Access of expression * expression
  | Block of expression list
  | IfBlock of expression * expression list
  | IfElseBlock of expression * expression list * expression list
  | Parameter of string * tTypes
  | FuncDecl of string * expression list * expression list * tTypes
  | FuncCall of string * expression list
  | FuncPiping of expression list
  | FuncComposition of expression list
  | FuncAnon of expression list * expression * tTypes

type root = expression list