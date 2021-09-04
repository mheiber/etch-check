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
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "lexing: empty token")
  Raised by primitive operation at file "lexing.ml", line 65, characters 15-37
  Called from file "lib/lexer.ml", line 596, characters 8-65
  Called from file "lib/parser.ml", line 894, characters 15-27
  Called from file "lib/parser.ml", line 916, characters 22-49
  Called from file "lib/etch.ml", line 11, characters 6-35
  Called from file "test/parse_test.ml", line 1, characters 18-97
  Called from file "test/parse_test.ml", line 18, characters 2-36
  Called from file "collector/expect_test_collector.ml", line 244, characters 12-19 |}]

let%expect_test "neg" =
  run "If true the true else false";
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "lexing: empty token")
  Raised by primitive operation at file "lexing.ml", line 65, characters 15-37
  Called from file "lib/lexer.ml", line 596, characters 8-65
  Called from file "lib/parser.ml", line 894, characters 15-27
  Called from file "lib/parser.ml", line 916, characters 22-49
  Called from file "lib/etch.ml", line 11, characters 6-35
  Called from file "test/parse_test.ml", line 1, characters 18-97
  Called from file "test/parse_test.ml", line 23, characters 2-35
  Called from file "collector/expect_test_collector.ml", line 244, characters 12-19 |}]
