type operator =
  Plus | Minus | Times | Divide | Modulo | Eq | Neq | Gt | Lt | Gte | Lte | And | Or | Not | Cons

type tTypes =
    TInt 
  | TUnit 
  | TBool 
  | TString 
  | TChar 
  | TTuple of tTypes list
  | TList of tTypes
  | TFloat
  | TMap of tTypes * tTypes
  | TFunction of tTypes list * tTypes

type parameter = string * tTypes

type expression =
    IntLiteral of int
  | FloatLiteral of float
  | BoolLiteral of bool
  | StringLiteral of string
  | CharLiteral of char
  | UnitLiteral
  | IdLiteral of string
  | TupleLiteral of expression list
  | ListLiteral of expression list
  | MapLiteral of (expression * expression) list
  | Binop of expression * operator * expression
  | Unop of operator * expression
  | TypedAssign of string * expression * tTypes
  | Assign of string * expression
  | Reassign of string * expression
  | TupleAccess of expression * expression
  | ListAccess of expression * expression
  | IfBlock of expression * expression list
  | IfElseBlock of expression * expression list * expression list
  | Call of expression * (expression list)
  | TypedFuncDecl of string * parameter list * expression list * tTypes
  | FuncDecl of string * parameter list * expression list
  | TypedFuncAnon of parameter list * expression * tTypes
  | FuncAnon of parameter list * expression
  | FuncPipe of expression * expression
  | FuncComposition of expression * expression

type root = expression list
