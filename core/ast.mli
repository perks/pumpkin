type operator = 
  Plus | Minus | Times | Divide | Modulo

type types = TNum | TUnit

type expression =
    Num_Literal of string
  | Id of string
  | Binop of expression * operator * expression
  
type statement = 
    Assign of string * expression
  | TAssign of string * types * expression

type program = {
  expression : expression list;
  statement : statement list;
}
