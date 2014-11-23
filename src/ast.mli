type operator = 
  Plus | Minus | Times | Divide | Modulo

type expression =
    NumLiteral of string
  | Binop of expression * operator * expression
  | Unit

type program = expression list
