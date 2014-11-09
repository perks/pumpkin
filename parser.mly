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
   | body
   | block TERMINATOR

body:
	 line
	|body TERMINATOR line
	|body TERMINATOR

block:
	INDENT body DEDENT
	|body TERMINATOR line
	|body

line:
	|expr
	|statement

statement:
	return
	|comment
	|terminal_statement

expr:
	func

return:


