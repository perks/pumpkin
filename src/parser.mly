%{ open Ast %}

%token TERMINATOR INDENT DEDENT
%token LPAREN RPAREN COLON COMMA LBRACK RBRACK TYPEARROW
%token PLUS MINUS TIMES DIVIDE MODULO EQ NEQ GT LT GTE LTE AND OR NOT
%token UMINUS UPLUS
%token VAL ASSIGN
%token IF ELSE
%token TINT TUNIT TBOOL TSTRING TCHAR TTUPLE TLIST
%token <string> ID
%token <int> INT
%token <int> DEDENT_COUNT
%token <bool> BOOL
%token <string> STRING
%token <char> CHAR
%token UNIT
%token EOF
%token <int> DEDENT_EOF

%nonassoc TINT TUNIT TBOOL TSTRING TCHAR TTUPLE TLIST
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LTE GTE
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right UMINUS UPLUS
%right NOT

%start root
%type < Ast.root > root

%%
root:
    /* nothing */ { [] }
  | root expression TERMINATOR { $2::$1 }
  
expression:
    LPAREN expression RPAREN             { $2 }
  | indent_block                         { Block($1) }
  | controlflow                          { $1 }
  | VAL ID COLON types ASSIGN expression { TypeAssing($2, $6, $4) }
  | VAL ID ASSIGN expression             { Assing($2, $4) }
  | expression PLUS   expression         { Binop($1, Plus, $3) }
  | expression MINUS  expression         { Binop($1, Minus, $3) }
  | expression TIMES  expression         { Binop($1, Times, $3) }
  | expression DIVIDE expression         { Binop($1, Divide, $3) }
  | expression MODULO expression         { Binop($1, Modulo, $3) }
  | expression EQ     expression         { Binop($1, Eq, $3) }
  | expression NEQ    expression         { Binop($1, Neq, $3) }
  | expression GT     expression         { Binop($1, Gt, $3) }
  | expression LT     expression         { Binop($1, Lt, $3) }
  | expression LTE    expression         { Binop($1, Lte, $3) }
  | expression GTE    expression         { Binop($1, Gte, $3) }
  | expression AND    expression         { Binop($1, And, $3) }
  | expression OR     expression         { Binop($1, Or, $3) }
  | MINUS expression %prec UMINUS        { Uniop(Minus, $2) }
  | PLUS expression %prec UPLUS          { Uniop(Plus, $2) }
  | NOT expression                       { Uniop(Not, $2) }
  | INT                                  { IntLiteral($1) }
  | BOOL                                 { BoolLiteral($1) }
  | STRING                               { StringLiteral($1) }
  | CHAR                                 { CharLiteral($1) }
  | UNIT                                 { UnitLiteral }
  | LPAREN exp_listing RPAREN            { TupleLiteral($2) }
  | LBRACK exp_listing RBRACK            { ListLiteral($2) }
  | TLIST LPAREN exp_listing RPAREN      { ListLiteral($3) }

exp_listing:
    expression COMMA { [$1] }
  | expression COMMA exp_listing_tail { $1::$3 }

exp_listing_tail:
    expression COMMA exp_listing_tail { $1::$3 }
  | expression  { [$1] }

expression_block:
    expression TERMINATOR { [$1] }
  | expression_block expression TERMINATOR { $2::$1 }

indent_block:
  INDENT expression_block DEDENT { List.rev $2 }

controlflow:
    if_statement indent_block  { IfBlock($1, $2) }
  | if_statement indent_block
    else_statement indent_block { IfElseBlock($1, $2, $4) }

if_statement:
  IF expression COLON TERMINATOR { $2 }

else_statement:
  ELSE COLON TERMINATOR { }



types:
    TINT       { TInt }
  | TBOOL      { TBool }
  | TSTRING    { TString }
  | TCHAR      { TChar }
  | TUNIT      { TUnit }
  | TTUPLE     { TTuple }
  | TLIST      { TList }
