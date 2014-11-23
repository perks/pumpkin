type operator = 
  Plus | Minus | Times | Divide | Modulo | Eq| Neq | Gt | Gte | Lt | Lte

type types = TNum | TUnit | TBool

type expression =
    Num_Literal of string
  | Bool_Literal of bool
  | Id of string
  | Binop of expression * operator * expression
  
type statement = 
    Assign of string * expression
  | TAssign of string * types * expression

type program = {
  expression : expression list;
  statement : statement list;
}
