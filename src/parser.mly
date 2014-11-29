%{ open Ast %}

%token TERMINATOR INDENT DEDENT
%token LPAREN RPAREN COLON
%token PLUS MINUS TIMES DIVIDE MODULO EQ NEQ GT LT GTE LTE
%token VAL ASSIGN
%token TNUM TUNIT TBOOL TSTRING
%token <string> ID
%token <int> NUM
%token <int> DEDENT_COUNT
%token <bool> BOOL
%token <string> STRING
%token UNIT
%token EOF

%nonassoc TNUM TUNIT TBOOL
%right ASSIGN
%left EQ NEQ
%left LT GT LTE GTE
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
  | expression EQ     expression   { Binop($1, Eq, $3) }
  | expression NEQ    expression   { Binop($1, Neq, $3) }
  | expression GT     expression   { Binop($1, Gt, $3) }
  | expression LT     expression   { Binop($1, Lt, $3) }
  | expression LTE    expression   { Binop($1, Lte, $3) }
  | expression GTE    expression   { Binop($1, Gte, $3) }
  | VAL ID COLON types ASSIGN expression { TypeAssing($2, $6, $4) }
  | NUM                            { IntLiteral($1) }
  | BOOL                           { BoolLiteral($1) }
  | STRING                         { StringLiteral($1) }
  | UNIT                           { UnitLiteral }
/*  | VAL ID ASSIGN expression             { Assign($2, $4) }*/

types:
    TNUM       { TNum }
  | TBOOL      { TBool }
  | TSTRING    { TString}
  | TUNIT      { TUnit }
