{
    open Parser
    open Utils

    let indent_stack = Stack.create()
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let id  = alpha (alpha | digit | '_')*
let num = ['-' '+']? digit* (['.'] digit+)?

rule token = parse
    "//"         { single_comment lexbuf }
    | "/*"         { block_comment lexbuf }
    | ['\r' '\n']+ { indent lexbuf }
    | [' ' '\t']    { token lexbuf }

    | '(' { LPAREN }
    | ')' { RPAREN }

    | ':' { COLON }
    | '=' { ASSIGN }

    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { TIMES }
    | '/' { DIVIDE }
    | '%' { MODULO }
    
    | "val"         { VAL }

    | "Num"     { NUM }
    | "Unit"    { UNIT }
    
    | id as lxm     { ID(lxm) }

    | num as lxm    { NUM_LITERAL(lxm) }

    | eof           { EOF }
    | _ as illegal     { raise (Failure("illegal character " ^ Char.escaped illegal)) }

and single_comment = parse
    '\n' { token lexbuf }
    | _ { comment lexbuf }

and block_comment = parse
    "*/" { token lexbuf }
    | _ { comment lexbuf }

and indent = parse
    [' ' '\t']* as indt { token lexbuf }
