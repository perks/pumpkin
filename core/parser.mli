type token =
  | LPAREN
  | RPAREN
  | COLON
  | ASSIGN
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MODULO
  | VAL
  | TNUM
  | TUNIT
  | ID of (string)
  | NUM of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  Ast.program 
