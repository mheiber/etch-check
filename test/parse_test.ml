let parse example =
  example |> Lexing.from_string |> Etch.parse |> Etch.Node.show_expr
  |> print_endline

let%expect_test _ =
  parse "let x := 5 in let y := 6 in x + y";
  [%expect {|
    (let x := 5 in (let y := 6 in (x + y))) |}]

let%expect_test _ =
  parse "if true then true else false";
  [%expect {|
    (if true then true else false) |}]

let%expect_test "neg" =
  parse "If true then true else false";
  [%expect {| (Error: unexpected char 'I') |}]

let%expect_test "neg" =
  parse "if true the true else false";
  [%expect {| (Error: syntax error) |}]
