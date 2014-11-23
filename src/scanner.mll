{
    open Parser
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
    
    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { TIMES }
    | '/' { DIVIDE }
    | '%' { MODULO }
    
    | "Unit" { UNIT }
    
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
    [' ' '\t']* { token lexbuf }
