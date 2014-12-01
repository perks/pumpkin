{
    open Parser
    
    let lineno = ref 1
    let indent_stack = Stack.create ()
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let id  = alpha (alpha | digit | '_')*
let string = '"' [^ '"' '\\']*('\\' '.' [^ '"' '\\']*)* '"'
let char = ''' ( alpha | digit ) '''
let float = digit+ ['.'] digit+
let int = digit+
let whitespace = [' ' '\t']
let return = ['\r' '\n']

rule token = parse
      "//"           { single_comment lexbuf }
    | "/*"         { block_comment lexbuf }
    | return+ { incr lineno; indent lexbuf }
    | whitespace   { token lexbuf }
    
    | '(' { LPAREN }
    | ')' { RPAREN }
    | '[' { LBRACK }
    | ']' { RBRACK }
    | '=' { ASSIGN }
    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { TIMES }
    | '/' { DIVIDE }
    | '%' { MODULO }
    | ':' { COLON }
    | ',' { COMMA }
    | '$'(int as lxm) { TUPALACC(int_of_string lxm) }
    
    | "->"         { TYPEARROW }
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
    
    | "val"        { VAL }
    | "Int"        { TINT }
    | "Float"      { TFLOAT }
    | "String"     { TSTRING }
    | "Unit"       { TUNIT }
    | "Char"       { TCHAR }
    | "Tuple"      { TTUPLE }
    | "List"       { TLIST }
    
    | "False"       { BOOL(false) }
    | "True"        { BOOL(true) }
    | "^"           { UNIT }
    | int as lxm    { INT(int_of_string lxm) }
    | float as lxm  { FLOAT(float_of_string lxm) }
    | string as lxm { STRING(lxm) }
    | id as lxm     { ID(lxm) }
    | char as lxm   { CHAR(String.get lxm 1)}
    
    | eof           
      {
        let indent_length = Stack.length indent_stack - 1 in 
        DEDENT_EOF(indent_length)
      }
    | _ as illegal  
      { 
        raise (Utils.IllegalCharacter(illegal, !lineno))
      }

and single_comment = parse
    '\n' { token lexbuf }
    | _ { single_comment lexbuf }

and block_comment = parse
    "*/" { token lexbuf }
    | _ { block_comment lexbuf }

and indent = parse
    whitespace*return+ { incr lineno; indent lexbuf }
  | whitespace*eof
    {
      let indent_length = Stack.length indent_stack - 1 in 
      DEDENT_EOF(indent_length)
    }
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
          if count = - 1 then raise (Utils.IndentationError !lineno)
          else DEDENT_COUNT(count)
      }

{
  Stack.push 0 indent_stack
}
