type action = Raw | Ast | Interpret | Compile

let _ =
  if Array.length Sys.argv < 2 then
    print_string (
      "Usage: pmkn [required-option] <source file>\n" ^
        "required-option:\n" ^
        "\t-r: Prints raw output from parser\n" ^
        "\t-a: Pretty prints Ast\n" ^
        "\t-i: Runs interpreter\n" ^
        "\t-c: Compiles to Java\n"
    )
  else
    let action = List.assoc Sys.argv.(1) [ ("-r", Raw);
                                           ("-a", Ast);
                                           ("-i", Interpret);
                                           ("-c", Compile) ] and
    filename = Sys.argv.(2) in
    let file_in = open_in filename in
    try 
      let lexbuf = Lexing.from_channel file_in in
      let program = Parser.root Scanner.token lexbuf in
      match action with 
          Raw -> print_string(Utils.program_to_string program)
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
      | _ -> print_string("Error\n")