%{ open Ast %}

%token LPAREN RPAREN
%token PLUS MINUS TIMES DIVIDE MODULO
%token <string> NUM
%token UNIT
%token EOF

%left PLUS MINUS
%left TIMES DIVIDE MODULO
%left LPAREN RPAREN

%start program
%type < Ast.program > program

%%
program:
    /* nothing */ { [] }
  | expression    { [$1] }
  
expression:
    LPAREN expression RPAREN  { $2 }
  | expression PLUS   expression   { Binop($1, Plus, $3) }
  | expression MINUS  expression   { Binop($1, Minus, $3) }
  | expression TIMES  expression   { Binop($1, Times, $3) }
  | expression DIVIDE expression   { Binop($1, Divide, $3) }
  | expression MODULO expression   { Binop($1, Modulo, $3) }
  | NUM                            { NumLiteral($1) }
  | UNIT                           { Unit }

