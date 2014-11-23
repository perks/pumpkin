{
    open Parser
    open Utils

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
    
    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { TIMES }
    | '/' { DIVIDE }
    | '%' { MODULO }
    
    | "val"         { VAL }

    | "Num"     { TNUM }
    | "Unit"    { TUNIT }
    
    | num as lxm    { NUM(lxm) }

    
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
          let decrement =
            let rec helper inc =
              if (Stack.top indent_stack) > indt_len then
                Stack.pop indent_stack
                helper (inc + 1)
              else if (Stack.top indent_stack) < indt_len then -1
              else inc
            in helper 0
          in
          if decrement = - 1 then raise (Failure "Indent mismatch")
          else DEDENT(decrement)
      }

{
  Stack.push 0 indent_stack
}

