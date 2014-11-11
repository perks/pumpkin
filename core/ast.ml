type operator =
  Plus | Minus | Times | Divide | Modulo |
  Eq | Neq | And | Or | Not |
  Gt | Lt | Gte | Lte


type expression =
    Num_Literal of string
  | Id of string
  | Assign of string * expression
  | Binop of expression * operator * expression

