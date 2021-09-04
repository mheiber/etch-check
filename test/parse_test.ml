let run example =
  example |> Lexing.from_string |> Etch.parse |> Etch.Node.show_expr
  |> print_endline

let%expect_test _ =
  run "let x := 5 in let y := 6 in x + y";
  [%expect {|
    (let x := 5 in (let y := 6 in (x + y))) |}]

let%expect_test _ =
  run "if true then true else false";
  [%expect {|
    (if true then true else false) |}]

let%expect_test "neg" =
  run "If true then true else false";
  [%expect {| (Error: unexpected char 'I') |}]

let%expect_test "neg" =
  run "if true the true else false";
  [%expect {| (Error: syntax error) |}]
