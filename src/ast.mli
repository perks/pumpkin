type operator =
  Plus | Minus | Times | Divide | Modulo | Eq | Neq | Gt | Lt | Gte | Lte | And | Or | Not | Cons

type tTypes =
    TInt 
  | TUnit 
  | TBool 
  | TString 
  | TChar 
  | TTuple of tTypes
  | TList of tTypes
  | TFloat
  | TAlgebraic of string
  | TMap of tTypes * tTypes
  | TFunction of tTypes * tTypes

type parameter = string * tTypes

type variant_decl = 
    VariantEmpty of string
  | VariantProduct of string * parameter list

type algebraic_decl = 
    AlgebraicEmpty of string
  | AlgebraicProduct of string * parameter list
  | AlgebraicSum of string * variant_decl list

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
  | Wildcard
  | Binop of expression * operator * expression
  | Unop of operator * expression
  | TypedAssign of string * expression * tTypes
  | Assign of string * expression
  | Reassign of string * expression
  | TupleAccess of expression * expression
  | ListAccess of expression * expression
  | AlgebricAccess of expression * string
  | IfBlock of expression * expression list
  | IfElseBlock of expression * expression list * expression list
  | MatchBlock of expression * (expression * expression) list
  | Call of string * (expression list)
  | TypedFuncDecl of string * parameter list * expression list * tTypes
  | FuncDecl of string * parameter list * expression list
  | TypedAnonDecl of parameter list * expression * tTypes
  | AnonDecl of  parameter list * expression 
  
(*
  | FuncPiping of expression list
  | FuncComposition of expression list
*)

type root = expression list * algebraic_decl list
