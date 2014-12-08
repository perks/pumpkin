open Parser

let token_to_string = function
    TERMINATOR -> "TERMINATOR" | INDENT -> "INDENT"
  | DEDENT -> "DEDENT" | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN" | COLON -> "COLON"
  | COMMA -> "COMMA" | LBRACK -> "LBRACK"
  | RBRACK -> "RBRACK" | TYPEARROW -> "TYPEARROW" | DEFARROW ->  "DEFARROW"
  | FPIPE -> "FPIPE" | BPIPE -> "BPIPE" | LCOMPOSE -> "LCOMPOSE" | RCOMPOSE -> "RCOMPOSE"
  | PLUS -> "PLUS" | MINUS -> "MINUS"
  | TIMES -> "TIMES" | DIVIDE -> "DIVIDE"
  | MODULO -> "MODULO" | EQ -> "EQ"
  | NEQ -> "NEQ" | GT -> "GT"
  | LT -> "LT" | GTE -> "GTE"
  | LTE -> "LTE" | AND -> "AND"
  | OR -> "OR" | NOT -> "NOT"
  | UMINUS -> "UMINUS" | UPLUS -> "UPLUS"
  | VAL -> "VAL" | ASSIGN -> "ASSIGN" | DEF -> "DEF"
  | IF -> "IF" | ELSE -> "ELSE"
  | TINT -> "TINT" | TUNIT -> "TUNIT"
  | TBOOL -> "TBOOL" | TSTRING -> "TSTRING"
  | TCHAR -> "TCHAR" | TTUPLE -> "TTUPLE"
  | TLIST -> "TLIST"| TFLOAT -> "TFLOAT"
  | TYPE -> "TYPE" | EXTENDS -> "EXTENDS"
  | TMAP -> "TMAP"
  | UNIT -> "UNIT"
  | MATCH -> "MATCH"
  | EOF -> "EOF" 
  | ID(s) -> "ID(" ^ s ^ ")"
  | INT(i) -> "INT(" ^ string_of_int i ^ ")"
  | FLOAT(f) -> "FLOAT(" ^ string_of_float f ^ ")"
  | DEDENT_COUNT(i) -> "DEDENT_COUNT(" ^ string_of_int i ^ ")"
  | BOOL(b) -> "BOOL(" ^ (if b then "true" else "false") ^ ")"
  | STRING(s) -> "STRING(" ^ s ^ ")"
  | TUPLEACC -> "TUPLEACC" | ACCESSOR -> "ACCESSOR"
  | CHAR(c) -> "CHAR(" ^ Char.escaped c ^ ")"
  | DEDENT_EOF(i) -> "DEDENT_EOF(" ^ string_of_int i ^ ")"

(* Custom parser to account for dedent *)
let dedent_list_from_count count =
  let rec helper count dedent_list =
    if count > 0 then DEDENT::TERMINATOR::(helper (count-1) dedent_list)
    else dedent_list
  in helper count []

let build_token_list lexbuf = 
  let rec helper lexbuf token_list =
    match Scanner.token lexbuf with
        DEDENT_EOF(_) as eof -> eof::token_list
      | t -> t::(helper lexbuf token_list)
  in helper lexbuf []

let expand_token_list token_list =
  let rec expand = function
      INDENT::tail -> TERMINATOR::INDENT::(expand tail)
    | DEDENT_COUNT(c)::tail -> 
        TERMINATOR::(List.append (dedent_list_from_count c) (expand tail))
    | DEDENT_EOF(c)::tail -> 
        TERMINATOR::(List.append (dedent_list_from_count c) (expand (EOF::tail)))
    | head::tail -> head::(expand tail)
    | [] -> []
  in expand token_list

let clean_token_list token_list =
  let rec clean = function
    | TERMINATOR::ELSE::tail -> clean (ELSE::tail)
    | head::tail -> head::(clean tail)
    | [] -> []
  in clean token_list
  
let get_token_list lexbuf =
  let token_list = build_token_list lexbuf in
  let expanded_token_list = expand_token_list token_list in
  let cleaned_token_list = clean_token_list expanded_token_list in
  match cleaned_token_list with
      TERMINATOR::tail -> tail
    | _ as tl -> tl

let parser token_list =
  let token_list = ref(token_list) in
  let tokenizer _ =
    match !token_list with
        head :: tail -> 
          token_list := tail;
          head 
      | [] -> raise (Failure "EOF missing")
  in Parser.root tokenizer (Lexing.from_string "")