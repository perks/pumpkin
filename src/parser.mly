%{ open Ast %}

%token TERMINATOR INDENT DEDENT
%token LPAREN RPAREN
%token PLUS MINUS TIMES DIVIDE MODULO
%token VAL
%token TNUM TUNIT
%token <string> ID

%token <int> NUM
%token <int> DEDENT_COUNT
%token UNIT
%token EOF


%left PLUS MINUS
%left TIMES DIVIDE MODULO
%left LPAREN RPAREN

%start root
%type < Ast.root > root

%%
root:
    /* nothing */ { [] }
  | body { $1 }
  | block TERMINATOR { $1 }

body:
  | expression { [$1] }
  | body TERMINATOR expression { List.append $1 [$3] }
  | body TERMINATOR { $1 }

block:
  | INDENT body DEDENT { $2 }

expression:
    LPAREN expression RPAREN  { $2 }
  | expression PLUS   expression   { Binop($1, Plus, $3) }
  | expression MINUS  expression   { Binop($1, Minus, $3) }
  | expression TIMES  expression   { Binop($1, Times, $3) }
  | expression DIVIDE expression   { Binop($1, Divide, $3) }
  | expression MODULO expression   { Binop($1, Modulo, $3) }
