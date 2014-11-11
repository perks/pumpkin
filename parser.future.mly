%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE COMMA COLON ASSIGN
%token TUPALACC DEFARROW TYPEARROW GUARD
%token FPIPE BPIPE RCOMPOSE LCOMPOSE CONS
%token PLUS MINUS TIMES DIVIDE MODULO
%token EQ NEQ AND OR NOT GT LT GTE LTE
%token IF ELSE MATCH AS BOOL
%token VAL DEF
%token <float>  NUM
%token <string> STRING
%token <string> ID
%token EOF


%right BPIPE
%left FPIPE
%left COMPOSE
%left OR
%left AND
%right NOT
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%left CONS
%left LBRACE RBRACE
%left LPAREN RPAREN

%start program
%type < Ast.program> program

%%
program:
	UNIT
   | block
   | block block

block:
	INDENT body return DEDENT
	| INDENT func_def body return DEDENT
	| IF LPAREN boolexpr RPAREN COLON INDENT line DEDENT
	| IF LPAREN boolexpr RPAREN COLON INDENT body DEDENT ELSE COLON INDENT body DEDENT
	| body

body:
	line
	| line line

line:
	expr
	| statement

statement:
	comment
	| terminal_statement
	| VAL ID COLON TYPE ASSIGN expr

expr:
	LBRACE expr RBRACE
	| expr PLUS   expr
	| expr MINUS  expr
	| expr TIMES  expr
	| expr DIVIDE expr
	| expr MODULO expr
	| expr EQ     expr
	| expr NEQ    expr
	| expr GT     expr
	| expr LT     expr
	| expr LTE    expr
	| expr GTE    expr
	| TUPALACC
	| boolexpr
	| lit

boolexpr:
	boolexpr AND  boolexpr
	| boolexpr OR boolexpr
	| NOT boolexpr
	| BOOL

lit:
	NUM
	| STRING

return:
	expr
	| UNIT

func_def:
	DEF ID COLON TYPE DEFARROW
	| DEF ID LPAREN params RPAREN COLON TYPE DEFARROW

params:
	ID COLON TYPE
	| params COMMA params