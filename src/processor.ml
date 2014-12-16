open Parser

let line_number = ref 1
let last_token = ref EOF

(* Custom parser to account for dedent, carries line numbers from scanner *)
let dedent_list_from_count count ln =
  let rec helper count dedent_list =
    if count > 0 then (DEDENT, ln)::(TERMINATOR, ln)::(helper (count-1) dedent_list)
    else dedent_list
  in helper count []

let build_token_list lexbuf =
  let rec helper lexbuf token_list =
    let ln = !Scanner.lineno in
    match Scanner.token lexbuf with
        DEDENT_EOF(_) as eof -> (eof, ln)::token_list
      | t -> (t, ln)::(helper lexbuf token_list)
  in helper lexbuf []

let expand_token_list token_list =
  let rec expand = function
      (INDENT, ln)::tail -> (TERMINATOR, ln)::(INDENT, ln)::(expand tail)
    | (DEDENT_COUNT(c), ln)::tail -> 
        (TERMINATOR, ln)::(List.append (dedent_list_from_count c ln) (expand tail))
    | (DEDENT_EOF(c), ln)::tail -> 
        (TERMINATOR, ln)::(List.append (dedent_list_from_count c ln) (expand ((EOF, ln)::tail)))
    | head::tail -> head::(expand tail)
    | [] -> []
  in expand token_list

let clean_token_list token_list =
  let rec clean = function
    | (TERMINATOR, _)::(ELSE, ln)::tail -> clean ((ELSE, ln)::tail)
    | head::tail -> head::(clean tail)
    | [] -> []
  in clean token_list
  
let get_token_list lexbuf =
  let token_list = build_token_list lexbuf in
  let expanded_token_list = expand_token_list token_list in
  let cleaned_token_list = clean_token_list expanded_token_list in
  match cleaned_token_list with
      (TERMINATOR, _)::tail -> tail
    | _ as tl -> tl

let parser token_list =
  let token_list = ref(token_list) in
  let tokenizer _ =
    match !token_list with
      | (head, ln) :: tail -> 
          line_number := ln;
          last_token := head;
          token_list := tail;
          head
      | [] -> raise (Exceptions.MissingEOF)
  in
  let program = Parser.root tokenizer (Lexing.from_string "") in
  List.rev program