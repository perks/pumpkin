%{ open Ast %}

%token LPAREN RPAREN
%token PLUS MINUS TIMES DIVIDE MODULO
%token VAL
%token TNUM TUNIT TBOOL
%token <string> ID

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
  | expression EQ     expression   { Binop($1, Eq, $3) }
  | expression NEQ    expression   { Binop($1, Neq, $3) }
  | expression GT     expression   { Binop($1, Gt, $3) }
  | expression LT     expression   { Binop($1, Lt, $3) }
  | expression LTE    expression   { Binop($1, Lte, $3) }
  | expression GTE    expression   { Binop($1, Gte, $3) }
statement:
    VAL ID COLON types ASSIGN expression { TAssign($2, $4, $6) }
  | VAL ID ASSIGN expression             { Assign($2, $4) }

types:
    TUNIT { TUnit }
  | TNUM  { TNum }
  | TBOOL { TBool }

