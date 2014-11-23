open Sast
open Ast

let run (prog : Sast.aProgram) : unit =
  let rec eval_expr (expr : Sast.aExpression) : unit =
    match expr with
      AnIntLiteral(n, _) -> print_string(n)
    | AUnit (_) -> print_string("Unit")
    | ABinop(e1, op, e2, _) ->
      print_char('(');
      eval_expr e1;
      (
        match op with
          Plus -> print_char('+')
        | Minus -> print_char('-')
        | Times -> print_char('*')
        | Divide -> print_char('/')
        | Modulo -> print_char('%')
      );
      eval_expr e2;
      print_char(')');
  in
  List.iter (fun expr -> (eval_expr expr)) prog