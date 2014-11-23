type operator =
  Plus | Minus | Times | Divide | Modulo

type expression =
    IntLiteral of int
  | Binop of expression * operator * expression
  | Unit

type 'a body = ('a expression) list
type 'a block = 'a body

type root = expression body
