%{ open Ast %}

%token LPAREN RPAREN
%token COLON ASSIGN
%token PLUS MINUS TIMES DIVIDE MODULO
%token VAL
%token NUM UNIT
%token <string> ID
%token <string> NUM_LITERAL
%token EOF

%right ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%left LPAREN RPAREN

%start program
%type < Ast.program > program

%%
program:
    /* nothing */ { }
  | expression { }
  
expression:
    LPAREN expr RBRACE  { $2 }
  | expr PLUS   expr    { Binop($1, Plus, $3) }
  | expr MINUS  expr    { Binop($1, Minus, $3) }
  | expr TIMES  expr    { Binop($1, Times, $3) }
  | expr DIVIDE expr    { Binop($1, Divide, $3) }
  | expr MODULO expr    { Binop($1, Modulo, $3) }

