%{ open Ast %}

%token LPAREN RPAREN
%token COLON ASSIGN
%token PLUS MINUS TIMES DIVIDE MODULO
%token VAL
%token TNUM TUNIT
%token <string> ID
%token <string> NUM
%token EOF

%right ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%left LPAREN RPAREN

%start program
%type < Ast.program > program

%%
program:
    /* nothing */ { {expression=[]; statement=[];} }
  | expression    { {expression=$1::[]; statement=[];} }
  | statement     { {expression=[]; statement=$1::[];} }
  
expression:
    LPAREN expression RPAREN  { $2 }
  | expression PLUS   expression   { Binop($1, Plus, $3) }
  | expression MINUS  expression   { Binop($1, Minus, $3) }
  | expression TIMES  expression   { Binop($1, Times, $3) }
  | expression DIVIDE expression   { Binop($1, Divide, $3) }
  | expression MODULO expression   { Binop($1, Modulo, $3) }
  
statement:
    VAL ID COLON types ASSIGN expression { TAssign($2, $4, $6) }
  | VAL ID ASSIGN expression             { Assign($2, $4) }

types:
    TUNIT { TUnit }
  | TNUM  { TNum }

