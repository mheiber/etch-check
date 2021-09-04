open Etch.Node

let print_step step =
  let step_str = show_expr step in
  print_endline step_str;
  print_endline "----------------"

let example =
  Let ("x", Int 2, If (Eq (Plus (Var "x", Var "x"), Int 4), Var "x", Int 2))

let _ = List.iter print_step @@ Etch.check example
