{
    open Parser
    
    let lineno = ref 1
    let indent_stack = Stack.create ()
    
    let get_eof () = 
      let indent_length = Stack.length indent_stack - 1 in 
      DEDENT_EOF(indent_length)
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let id  = alpha (alpha | digit | '_')*
let string = '"' [^ '"' '\\' '\n' '\r' '\t']* ('\\' [^ '\n' '\r' '\t'] [^ '"' '\\']*)* '"'
let char = ''' ( alpha | digit ) '''
let float = digit+ ['.'] digit+
let int = digit+
let whitespace = [' ' '\t']
let return = '\n' | "\r\n"

rule token = parse
    whitespace* "//"         { single_comment lexbuf }
  | whitespace* "/*"         { block_comment lexbuf }
  
  | return       { incr lineno; indent lexbuf }
  | whitespace   { token lexbuf }
  
  | '('  { LPAREN }
  | ')'  { RPAREN }
  | '['  { LBRACK }
  | ']'  { RBRACK }
  | '='  { ASSIGN }
  | '+'  { PLUS }
  | '-'  { MINUS }
  | '*'  { TIMES }
  | '/'  { DIVIDE }
  | '%'  { MODULO }
  | "::" { CONS }
  | ':'  { COLON }
  | ','  { COMMA }
  | '$'  { TUPLEACC }
  
  | "->"         { TYPEARROW }
  | "=>"         { DEFARROW }
  | "if"         { IF }
  | "else"       { ELSE }
  
  | "is"  | "==" { EQ }
  | "and" | "&&" { AND }
  | "or"  | "||" { OR }
  | "not" | '!'  { NOT }
  | "!="         { NEQ }
  | '>'          { GT }
  | '<'          { LT }
  | ">="         { GTE }
  | "<="         { LTE }
  
  | "val"         { VAL }
  | "def"         { DEF }

  | "Int"        { TINT }
  | "Float"      { TFLOAT }
  | "String"     { TSTRING }
  | "Unit"       { TUNIT }
  | "Char"       { TCHAR }
  | "Tuple"      { TTUPLE }
  | "List"       { TLIST }
  | "Map"        { TMAP }
  | "Bool"       { TBOOL }
  
  | "|>" { FPIPE }
  | "<|" { BPIPE }
  | ">>" { RCOMPOSE }
  | "<<" { LCOMPOSE }
  
  | "|>" whitespace* return { incr lineno; FPIPE }
  | "<|" whitespace* return { incr lineno; BPIPE }
  | ">>" whitespace* return { incr lineno; RCOMPOSE }
  | "<<" whitespace* return { incr lineno; LCOMPOSE }
  
  | "False"       { BOOL(false) }
  | "True"        { BOOL(true) }
  | "()"          { UNIT }
  | int as lxm    { INT(int_of_string lxm) }
  | float as lxm  { FLOAT(float_of_string lxm) }
  | string as lxm { STRING(lxm) }
  | id as lxm     { ID(lxm) }
  | char as lxm   { CHAR(String.get lxm 1)}
  
  | eof { get_eof() }
  | '"' { raise (Exceptions.UnmatchedQuotation(!lineno)) }
  | _ as illegal  
    { 
      raise (Exceptions.IllegalCharacter(illegal, !lineno))
    }

and single_comment = parse
    return  { incr lineno; indent lexbuf }
  | eof { get_eof() }
  | _   { single_comment lexbuf }

and block_comment = parse
    return { incr lineno; block_comment lexbuf }
  | "*/" { token lexbuf }
  | _    { block_comment lexbuf }

and indent = parse
    whitespace* return { incr lineno; indent lexbuf }
  | whitespace* "//"   { single_comment lexbuf }
  | whitespace* "/*"   { block_comment lexbuf }
  | whitespace* "|>"   { incr lineno; FPIPE }
  | whitespace* "<|"   { incr lineno; BPIPE }
  | whitespace* ">>"   { incr lineno; RCOMPOSE }
  | whitespace* "<<"   { incr lineno; LCOMPOSE }
  | whitespace* eof    { get_eof() }
  | whitespace* as indt
      {
        let indt_len = (String.length indt) in
        let top_len = (Stack.top indent_stack) in
        if indt_len > top_len then
          begin
          Stack.push indt_len indent_stack;
          INDENT
          end
        else if indt_len = top_len then 
          TERMINATOR
        else
          let count = 
            let rec helper inc =
                if (Stack.top indent_stack) > indt_len then
                    begin
                    ignore(Stack.pop indent_stack);
                    helper (inc + 1)
                    end
                else if (Stack.top indent_stack) < indt_len then -1
                else inc
            in helper 0
          in 
          if count = - 1 then raise (Exceptions.IndentationError !lineno)
          else DEDENT_COUNT(count)
      }

{
  Stack.push 0 indent_stack
}
