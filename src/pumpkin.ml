type action = Tokens | Raw | Ast | Interpret | Compile

let _ =
  if Array.length Sys.argv < 2 then
    print_string (
      "Usage: pmkn [required-option] <source file>\n" ^
        "required-option:\n" ^
        "\t-t: Prints token stream\n" ^
        "\t-r: Prints raw output from parser\n" ^
        "\t-a: Pretty prints Ast\n" ^
        "\t-i: Runs interpreter\n" ^
        "\t-c: Compiles to Java\n"
    )
  else
    let action = List.assoc Sys.argv.(1) [ ("-t", Tokens);
                                           ("-r", Raw);
                                           ("-a", Ast);
                                           ("-i", Interpret);
                                           ("-c", Compile) ] and
    filename = Sys.argv.(2) in
    let file_in = open_in filename in
    try 
      let lexbuf = Lexing.from_channel file_in in
      let token_list = Processor.get_token_list lexbuf in
      match action with 
          Tokens -> 
            print_string (String.concat " " (List.map Processor.token_to_string token_list) ^ "\n")
        | Raw ->
          let program = List.rev (Processor.parser token_list) in
            print_string (Utils.program_to_string program)
        | Ast -> print_string("\nAst\n")
        | Interpret -> print_string("\nInterpret\n")
        | Compile -> print_string("\nCompile\n")
    with
        Utils.IllegalCharacter(c, ln) -> 
          print_string
          (
            "In \"" ^ filename ^ "\", Illegal Character, '" ^ 
            Char.escaped c ^ "', line " ^ string_of_int ln ^ "\n"
          )
      | Utils.IndentationError(ln) -> 
        print_string("IndentationError, line\n" ^ string_of_int ln ^ "\n")