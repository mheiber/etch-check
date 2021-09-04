open Etch.Node

let%expect_test _ =
  let parsed =
    Etch.parse @@ Lexing.from_string "let x := 5 in let y := 6 in x + y"
  in
  print_endline @@ show_expr parsed;
  [%expect {|
    (let x := 5 in (let y := 6 in (x + y))) |}]

let%expect_test _ =
  let parsed =
    Etch.parse @@ Lexing.from_string "if true then true else false"
  in
  print_endline @@ show_expr parsed;
  [%expect {|
    (if true then true else false) |}]
