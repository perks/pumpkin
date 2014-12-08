%{ open Ast %}

%token TERMINATOR INDENT DEDENT
%token LPAREN RPAREN COLON COMMA LBRACK RBRACK TYPEARROW DEFARROW
%token FPIPE BPIPE RCOMPOSE LCOMPOSE
%token PLUS MINUS TIMES DIVIDE MODULO EQ NEQ GT LT GTE LTE AND OR NOT
%token UMINUS UPLUS
%token VAL ASSIGN DEF
%token IF ELSE
%token TYPE EXTENDS
%token MATCH SELECTION
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

%nonassoc MATCH DEFARROW
%left BPIPE
%left FPIPE
%left FCOMPOSE
%right RCOMPOSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LTE GTE
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right UMINUS UPLUS
%right NOT
%left TUPLEACC ACCESSOR

%start root
%type < Ast.root > root

%%
root:
    /* nothing */ { [], [] }
  | root expression TERMINATOR { $2::(fst $1), snd $1 }
  | root algebraic_decl TERMINATOR  { fst $1, $2::(snd $1) }

expression:
    LPAREN expression RPAREN             { $2 }
  | indent_block                         { Block($1) }
  | controlflow                          { $1 }
  | match_statement                       { $1 }
  | assignment                           { $1 }
  | binop                                { $1 }
  | unop                                 { $1 }
  | literal                              { $1 }
  | func_declaration                     { $1 }
  | func_piping                          { $1 }
  | funcs                                { $1 }

funcs:
    func_composition                     { $1 }
  | func_calling                         { $1 }
  | func_anon                            { $1 }

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

match_statement:
    expression MATCH COLON TERMINATOR match_block { MatchBlock($1, $5) }

match_block:
    INDENT matches DEDENT TERMINATOR { $2 }

matches:
    expression DEFARROW expression TERMINATOR { ($1, $3)::[] }
  | matches SELECTION expression DEFARROW expression TERMINATOR { ($3, $5)::$1 }

assignment:
    VAL ID COLON types ASSIGN expression { TypeAssing($2, $6, $4) }
  | VAL ID ASSIGN expression             { Assing($2, $4) }

func_declaration:
    DEF ID LPAREN parameters RPAREN COLON types DEFARROW TERMINATOR indent_block  { FuncDecl($2, $4, $10, $7) }
  | DEF ID COLON types DEFARROW TERMINATOR indent_block                           { FuncDecl($2, [], $7, $4) }
  | DEF ID LPAREN parameters RPAREN COLON types DEFARROW LPAREN expression RPAREN { FuncDecl($2, $4, [$10], $7) }
  | DEF ID COLON types DEFARROW LPAREN expression RPAREN                          { FuncDecl($2, [], [$7], $4) }

func_calling:
    ID LPAREN literal_listing RPAREN          { FuncCall($1, $3) }
  | ID LPAREN RPAREN                          { FuncCall($1, []) }

func_anon:
  LPAREN parameters DEFARROW expression RPAREN COLON types  { FuncAnon($2, $4, $7)}

func_piping:
    func_piping_list                              { FuncPiping($1) }

func_composition:
    func_composition_list                         { FuncComposition($1) }

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
  | expression TUPLEACC expression       { TupleAccess($1, $3)}
  | LBRACK exp_listing RBRACK            { ListLiteral($2) }
  | TLIST LPAREN exp_listing RPAREN      { ListLiteral($3) }
  | TMAP LPAREN map_list_regular RPAREN  { MapLiteral($3) }
  | LPAREN map_list_special RPAREN       { MapLiteral($2) }
  | expression ACCESSOR expression       { Access($1, $3) }
  | ID                                   { IdLiteral($1) }

map_list_regular:
    LPAREN map_item_regular RPAREN                          { [$2] }
  | map_list_regular COMMA LPAREN map_item_regular RPAREN   { $4::$1 }

map_item_regular:
    expression COMMA expression { $1, $3 }

map_list_special:
    LPAREN map_item_special RPAREN                        { [$2] }
  | map_list_special COMMA LPAREN map_item_special RPAREN { $4::$1 }

map_item_special:
  expression TYPEARROW expression { $1, $3 }

literal_listing:
    literal                                 { [$1] }
  | literal COMMA literal_listing           { $1::$3 }

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
    expression TERMINATOR                  { [$1] }
  | expression_block expression TERMINATOR { $2::$1 }

func_piping_list:
    expression FPIPE funcs      { List.append [$1] [$3] }
  | funcs BPIPE expression      { List.append [$3] [$1] }

func_composition_list:
    func_composition_list_tail RCOMPOSE func_calling   { List.append $1 [$3] }
  | func_composition_list_tail LCOMPOSE func_calling   { $3::$1 }
  | func_composition_list_tail RCOMPOSE func_anon      { List.append $1 [$3] }
  | func_composition_list_tail LCOMPOSE func_anon      { $3::$1 }

func_composition_list_tail:
    func_calling                                       { [$1] }
  | func_anon                                          { [$1] }
  | func_composition_list_tail RCOMPOSE func_calling   { List.append $1 [$3] }
  | func_composition_list_tail FCOMPOSE func_calling   { $3::$1 }
  | func_composition_list_tail RCOMPOSE func_anon      { List.append $1 [$3] }
  | func_composition_list_tail FCOMPOSE func_anon      { $3::$1 }

algebraic_decl:
    TYPE ID algrbraic_param_list_opt            { AlgebraicBase($2, List.rev $3) }
  | TYPE ID algrbraic_param_list_opt EXTENDS ID { AlgebraicDerived($2, $5, List.rev $3) }

algrbraic_param_list_opt:
    /* nothing */                      { [] }
  | LPAREN algrbraic_param_list RPAREN { $2 }

algrbraic_param_list:
    algrbraic_param                            { [$1] }
  | algrbraic_param_list COMMA algrbraic_param { $3::$1 }

algrbraic_param:
    ID COLON types { NativeParam($1, $3) }
  | ID COLON ID    { AlgebraicParam($1, $3) }
