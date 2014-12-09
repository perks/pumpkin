open Parser

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
  in 
  let program = Parser.root tokenizer (Lexing.from_string "") in
  List.rev (fst program), List.rev (snd program)