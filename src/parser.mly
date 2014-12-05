%{ open Ast %}

%token TERMINATOR INDENT DEDENT
%token LPAREN RPAREN COLON COMMA LBRACK RBRACK TYPEARROW DEFARROW
%token PLUS MINUS TIMES DIVIDE MODULO EQ NEQ GT LT GTE LTE AND OR NOT
%token UMINUS UPLUS
%token VAL ASSIGN DEF
%token IF ELSE
%token TINT TUNIT TBOOL TSTRING TCHAR TTUPLE TLIST TFLOAT
%token <string> ID
%token <int> INT
%token <int> DEDENT_COUNT
%token <bool> BOOL
%token <string> STRING
%token <char> CHAR
%token <float> FLOAT
%token <int> TUPLEACC
%token <int> LISTACC
%token UNIT
%token EOF
%token <int> DEDENT_EOF

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LTE GTE
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right UMINUS UPLUS
%right NOT
%left TUPLEACC LISTACC

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
  | assignment                           { $1 }
  | binop                                { $1 }
  | unop                                 { $1 }
  | literal                              { $1 }
  | func_declaration                     { $1 }
  | func_calling                         { $1 }

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

assignment: 
    VAL ID COLON types ASSIGN expression { TypeAssing($2, $6, $4) }
  | VAL ID ASSIGN expression             { Assing($2, $4) }

func_declaration:
    DEF ID LPAREN parameters RPAREN COLON types DEFARROW TERMINATOR indent_block { FuncDecl($2, $4, $10, $7) }
  | DEF ID COLON types DEFARROW TERMINATOR indent_block                          { FuncDecl($2, [], $7, $4) }

func_calling:
    ID LPAREN literal_listing_comma RPAREN    { FuncCall($1, $3) }

types:
    TINT       { TInt }
  | TBOOL      { TBool }
  | TSTRING    { TString }
  | TCHAR      { TChar }
  | TUNIT      { TUnit }
  | TTUPLE     { TTuple }
  | TLIST      { TList }
  | TFLOAT     { TFloat }

binop:
    expression PLUS   expression         { Binop($1, Plus, $3) }
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

unop:
    MINUS expression %prec UMINUS        { Unop(Minus, $2) }
  | PLUS expression %prec UPLUS          { Unop(Plus, $2) }
  | NOT expression                       { Unop(Not, $2) }

literal:
    INT                                  { IntLiteral($1) }
  | FLOAT                                { FloatLiteral($1) }
  | BOOL                                 { BoolLiteral($1) }
  | STRING                               { StringLiteral($1) }
  | CHAR                                 { CharLiteral($1) }
  | UNIT                                 { UnitLiteral }
  | LPAREN exp_listing RPAREN            { TupleLiteral($2) }
  | expression TUPLEACC                  { TupleAccess($1, $2)}
  | LBRACK exp_listing RBRACK            { ListLiteral($2) }
  | TLIST LPAREN exp_listing RPAREN      { ListLiteral($3) }
  | expression LISTACC                   { ListAccess($1, $2) }
  | ID                                   { IdLiteral($1) }

literal_listing_comma:
    literal COMMA                            { [$1] }
  | literal COMMA literal_listing_comma_tail { $1::$3 }

literal_listing_comma_tail:
    literal                                  { [$1] }
  | literal COMMA literal_listing_comma_tail { $1::$3 }

exp_listing:
    expression COMMA                  { [$1] }
  | expression COMMA exp_listing_tail { $1::$3 }

exp_listing_tail:
    expression                        { [$1] }
  | expression COMMA exp_listing_tail { $1::$3 }

parameters:
    ID COLON types                       { [Parameter($1, $3)] }
  | parameters COMMA ID COLON types      { Parameter($3, $5)::$1 }

expression_block:
    expression TERMINATOR { [$1] }
  | expression_block expression TERMINATOR { $2::$1 }
