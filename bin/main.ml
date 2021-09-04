open Etch.Node

let print_step step =
  let step_str = show_expr step in
  print_endline step_str;
  print_endline "----------------"

let run example =
  example |> Lexing.from_string |> Etch.parse |> Etch.check
  |> List.iter print_step

let _ = run "let x := 2 in (if ((x + x) = 4) then x else 2)"
