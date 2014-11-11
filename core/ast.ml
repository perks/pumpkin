type operator = 
  Plus | Minus | Times | Divide | Modulo

type expression =
    Num_Literal of string
  | Id of string
  | Assign of string * expression
  | Binop of expression * operator * expression
