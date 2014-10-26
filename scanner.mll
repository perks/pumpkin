{ 
    open Parser
    open Utils

    let indent_stack = Stack.create()

}

rule token = parse
    [' ' '\r'] { token lexbuf }
    | "//" { single_comment lexbuf }
    | "/*" { block_comment lexbuf }
    | ['\r' '\n']+ { indent lexbuf } (* check line numbers eventually *)
    | [' ' 't'] { token lexbuf }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | '[' { LBRACE }
    | ']' { RBRACE }
    | ',' { COMMA }
    | ':' { COLON }
    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { TIMES }
    | '/' { DIVIDE }
    | "is" | "==" { EQ }
    | "and" | "&&" { AND }
    | "or" | "||" { OR }
    | "not" |'!' { NOT }
    | '>' { GT }
    | '<' { LT }
    | ">=" { GTE }
    | "<=" { LTE }
    | "Int" { INT }
    | "Float" { FLOAT }
    | "String" { STRING }
    | "Boolean" { BOOLEAN }
    | "Unit" { UNIT }
    | "val" { VAL }
    | "var" { VAR }
    | "Tupal" { TUPAL }
    | "$"(['0'-'9']+ as lxm) { TUPALACC(lxm) }
    | "List" { LIST }
    | "Map" { MAP }
    | "def" { DEF }
    | "=>" { DEFARROW }
    | "->" { TYPEARROW }
    | "|>" { FPIPE }
    | "<|" { BPIPE }
    | ">>" { COMPOSE }
    | "::" { CONS }
    | "type" { TYPE }
    | "extends" { EXTENDS }
    | "if" { IF }
    | "else" { ELSE }
    | "match" { MATCH }
    | '|' { MATCHOR }
    | "asInt" { ASINT }
    | "asFloat" { ASFLOAT }
    | ['0'-'9']+ as lxm { INT_LIT(int_of_string lxm) }
    | ['0'-'9']+'.'['0'-'9']+ as lxm { FLOAT_LIT(float_of_string lxm) }
    | ['e' 'E']['+' '-']?['0'-'9']+ as lxm { EXP_LIT(int_of_string lxm) }
    | "\""([^'"']* as lxm)"\"" { STR_LIT(lxm) }
    | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }


and single_comment = parse
    '\n' { token lexbuf }
    | _ { comment lexbuf }

and block_comment = parse
    "*/" { token lexbuf }
    | _ { comment lexbuf }

and indent = parse
    [' ' '\t']* as indt
        {
            let indt_len = (String.length indt) in
            let top_len = (Stack.top indent_stack) in
            if indt_len > top_len then
                Stack.push indt_len indent_stack;
                INDENT
            else if indt_len = top_len then
                NEWLINE
            else
                let decrement =
                    let rec helper inc =
                        if (Stack.top indent_stack) > len then 
                            Stack.pop indent_stack;
                            helper (inc + 1)
                        else if (Stack.top indent_stack) < len then -1
                        else inc
                    in
                    helper 0
                DEINDENT(decrement)
        }
