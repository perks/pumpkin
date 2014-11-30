{
    open Parser
    
    let lineno = ref 1
    let indent_stack = Stack.create ()
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let id  = alpha (alpha | digit | '_')*
let string_chars = [^ '"' '\\' '#']*('\\' '.' [^ '"' '\\' '#']*)*
let string = '"' string_chars '"'
let char = ''' ( alpha | digit ) '''
let double = digit+ ['.'] digit+
let int = digit+
let whitespace = [' ' '\t']*

rule token = parse
    "//"           { single_comment lexbuf }
    | "/*"         { block_comment lexbuf }
    | ['\r' '\n']+ 
      { 
        incr lineno;
        indent lexbuf;
      }
    | [' ' '\t']    { token lexbuf }

    | '(' { LPAREN }
    | ')' { RPAREN }
    | '[' { LBRACK }
    | ']' { RBRACK }
    | '{' { LCBRACK }
    | '}' { RCBRACK }
    | '=' { ASSIGN }
    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { TIMES }
    | '/' { DIVIDE }
    | '%' { MODULO }
    | ':' { COLON }
    | ',' { COMMA }
    | '"' { QUOTE }
    | '#' { POUND }

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
    | "String"     { TSTRING }
    | "Unit"       { TUNIT }
    | "Char"       { TCHAR }
    | "Tuple"      { TTUPLE }
    | "List"       { TLIST }

    | int as lxm    { INT(int_of_string lxm) }
    | "False"       { BOOL(false) }
    | "True"        { BOOL(true) }
    | string as lxm { STRING(lxm)}
    | id as lxm     { ID(lxm) }
    | char as lxm   { CHAR(String.get lxm 1)}
    | "^"           { UNIT }
    | string_chars as lxm { STRINGCHARS(lxm)}

    | eof           { EOF }
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
    whitespace as indt
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
