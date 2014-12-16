%{ open Ast %}

%token TERMINATOR INDENT DEDENT
%token LPAREN RPAREN COLON COMMA LBRACK RBRACK TYPEARROW DEFARROW
%token FPIPE BPIPE RCOMPOSE LCOMPOSE
%token PLUS MINUS TIMES DIVIDE MODULO EQ NEQ GT LT GTE LTE AND OR NOT
%token UMINUS UPLUS
%token CONS
%token VAL ASSIGN DEF
%token IF ELSE
%token TYPE
%token MATCH SELECTION WILDCARD
%token TINT TUNIT TBOOL TSTRING TCHAR TTUPLE TLIST TFLOAT TMAP
%token <string> ID
%token <int> INT
%token <int> DEDENT_COUNT
%token <bool> BOOL
%token <string> STRING
%token <char> CHAR
%token <float> FLOAT
%token TUPLEACC ACCESSOR
%token UNIT
%token EOF
%token <int> DEDENT_EOF

%right ASSIGN
%nonassoc LPAREN UNIT
%right DEFARROW
%left LBRACK RBRACK
%left FPIPE
%right BPIPE
%left RCOMPOSE
%right LCOMPOSE
%left OR
%left AND
%left EQ NEQ
%left LT GT LTE GTE
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right CONS
%right UMINUS UPLUS
%right NOT
%left TUPLEACC ACCESSOR

%start root
%type < Ast.root > root

%%
root:
    /* nothing */                  { [], [] }
  | root expression TERMINATOR     { $2::(fst $1), snd $1 }
  | root algebraic_decl TERMINATOR { fst $1, $2::(snd $1) }

expression:
    LPAREN expression RPAREN             { $2 }
  | controlflow                          { $1 }
  | match_statement                      { $1 }
  | assignment                           { $1 }
  | binop                                { $1 }
  | unop                                 { $1 }
  | literal                              { $1 }
  | call                                 { $1 }
  | funct                                { $1 }
  | WILDCARD                             { Wildcard }

indent_block:
    INDENT expression_block DEDENT { List.rev $2 }

expression_block:
    expression TERMINATOR                  { [$1] }
  | expression_block expression TERMINATOR { $2::$1 }

controlflow:
    if_statement indent_block   { IfBlock($1, $2) }
  | if_statement indent_block
    else_statement indent_block { IfElseBlock($1, $2, $4) }

if_statement:
    IF expression COLON TERMINATOR { $2 }

else_statement:
    ELSE COLON TERMINATOR { }

match_statement:
    MATCH expression COLON TERMINATOR match_block { MatchBlock($2, $5) }

match_block:
    INDENT matches DEDENT { List.rev $2 }

matches:
    match_item         { $1::[] }
  | matches match_item { $2::$1 }

match_item:
    SELECTION expression DEFARROW expression TERMINATOR { ($2, $4) }
  | SELECTION expression DEFARROW TERMINATOR INDENT expression TERMINATOR DEDENT TERMINATOR { ($2, $6) }

assignment:
    VAL ID COLON types ASSIGN expression { TypedAssign($2, $6, $4) }
  | VAL ID ASSIGN expression             { Assign($2, $4) }
  | ID ASSIGN expression                 { Reassign($1, $3) }

types:
    TINT                                 { TInt }
  | TFLOAT                               { TFloat }
  | TBOOL                                { TBool }
  | TSTRING                              { TString }
  | TCHAR                                { TChar }
  | TUNIT                                { TUnit }
  | TTUPLE LBRACK type_list RBRACK       { TTuple($3) }
  | TLIST LBRACK types RBRACK            { TList($3) }
  | TMAP LBRACK types COMMA types RBRACK { TMap($3, $5) }
  | LPAREN funct_type RPAREN             { $2 }
  | ID                                   { TAlgebraic($1) }

funct_type:
    type_list DEFARROW types      { TFunction($1, $3) }
  | type_list DEFARROW funct_type { TFunction($1, $3) }

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
  | expression CONS   expression         { Binop($1, Cons, $3) }

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
  | LPAREN tupal_elements RPAREN         { TupleLiteral(List.rev $2) }
  | expression TUPLEACC expression       { TupleAccess($1, $3) }
  | LBRACK expression_list RBRACK        { ListLiteral($2) }
  | LBRACK RBRACK                        { ListLiteral([]) }
  | expression LBRACK expression RBRACK  { ListAccess($1, $3) }
  | LPAREN map_list RPAREN               { MapLiteral($2) }
  | expression ACCESSOR ID               { AlgebricAccess($1, $3)}
  | ID                                   { IdLiteral($1) }

map_list:
    map_item                { [$1] }
  | map_list COMMA map_item { $3::$1 }

map_item:
    expression TYPEARROW expression  { $1, $3 }

expression_list:
    expression                       { [$1] }
  | expression_list COMMA expression { $3::$1 }

tupal_elements:
    expression COMMA                     { [$1] }
  | tupal_elements_head expression       { $2::$1 }
  | tupal_elements_head expression COMMA { $2::$1 }

tupal_elements_head:
    expression COMMA                     { [$1] }
  | tupal_elements_head expression COMMA { $2::$1 }

algebraic_decl:
    TYPE ID                                              { AlgebraicEmpty($2) }
  | TYPE ID LPAREN parameter_list RPAREN                 { AlgebraicProduct($2, List.rev $4) }
  | TYPE ID ASSIGN TERMINATOR INDENT variant_list DEDENT { AlgebraicSum($2, List.rev $6) }

variant_list:
    SELECTION variant TERMINATOR              { [$2] }
  | variant_list SELECTION variant TERMINATOR { $3::$1}

variant:
    ID                               { VariantEmpty($1) }
  | ID LPAREN parameter_list RPAREN  { VariantProduct($1, List.rev $3) }

parameter_list:
    parameter                       { [$1] }
  | parameter_list COMMA parameter { $3::$1 }

parameter:
    ID COLON types { $1, $3 }

type_list:
    types                       { [$1] }
  | type_list COMMA types       { $3::$1 }

call:
    expression UNIT                          { Call($1, [UnitLiteral]) }
  | expression LPAREN expression_list RPAREN { Call($1, List.rev $3) }

funct:
    function_declaration { $1 }
  | function_anon        { $1 }
  | function_pipe        { $1 }
  | function_composition { $1 }

function_declaration:
    DEF ID function_parameters COLON types DEFARROW TERMINATOR indent_block { TypedFuncDecl($2, $3, $8, $5) }
  | DEF ID function_parameters COLON types DEFARROW expression              { TypedFuncDecl($2, $3, [$7], $5)}
  | DEF ID function_parameters DEFARROW TERMINATOR indent_block             { FuncDecl($2, $3, $6) }
  | DEF ID function_parameters DEFARROW expression                          { FuncDecl($2, $3, [$5]) }

function_parameters:
    /* nothing */                { ["()", TUnit] }
  | LPAREN parameter_list RPAREN { $2 }

function_anon:
    LPAREN parameter_list DEFARROW expression COLON types RPAREN { TypedFuncAnon($2, $4, $6) }
  | LPAREN parameter_list DEFARROW expression RPAREN             { FuncAnon($2, $4) }

function_pipe:
    expression FPIPE expression { FuncPipe($1, $3) }
  | expression BPIPE expression { FuncPipe($3, $1) }

function_composition:
    expression RCOMPOSE expression { FuncComposition($1, $3) }
  | expression LCOMPOSE expression { FuncComposition($3, $1) }

