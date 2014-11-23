{
    open Parser

    let indent_stack = Stack.create()
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let id  = alpha (alpha | digit | '_')*
let num = ['-' '+']? digit* (['.'] digit+)?
let whitespace = [' ' '\t']*

rule token = parse
    "//"         { single_comment lexbuf }
    | "/*"         { block_comment lexbuf }
    | ['\r' '\n']+ { indent lexbuf }
    | [' ' '\t']    { token lexbuf }

    | '(' { LPAREN }
    | ')' { RPAREN }

    | '=' { ASSIGN }
    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { TIMES }
    | '/' { DIVIDE }
    | '%' { MODULO }
    | ':' { COLON }

    
    | "is"  | "==" { EQ }
    | "!="         { NEQ }
    | '>'          { GT }
    | '<'          { LT }
    | ">="         { GTE }
    | "<="         { LTE }    
    | "val"         { VAL }
    | "Num"     { TNUM }
    | "Unit"    { TUNIT }

    | num as lxm    { NUM(int_of_string lxm) }
    | "False"       { BOOL(false) }
    | "True"        { BOOL(true) }
    | id as lxm     { ID(lxm) }


    | eof           { EOF }
    | _ as illegal     { raise (Failure("illegal character " ^ Char.escaped illegal)) }

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
        else if indt_len = top_len then TERMINATOR
        else
          let count = 
            let decrement_count = fun len stack ->
                let rec helper inc =
                    if (Stack.top stack) > len then
                        begin
                        Stack.pop stack
                        helper (inc + 1)
                        end
                    else if (Stack.top stack) < len then -1
                    else inc
                in helper 0
          in decrement_count indt_len indent_stack
          in 
          if count = - 1 then raise (Failure "Indent mismatch")
          else DEDENT(count)
      }

{
  Stack.push 0 indent_stack
}
