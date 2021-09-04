open Etch.Node

let print_step step =
  let step_str = show_expr step in
  print_endline step_str;
  print_endline "----------------"

let run example =
  example |> Lexing.from_string |> Etch.parse |> Etch.check
  |> List.iter print_step

let%expect_test _ =
  run "3";
  [%expect {|
    3
    ----------------
    Int
    ---------------- |}]

let%expect_test _ =
  run "let x := 2 in x";
  [%expect
    {|
    (let x := 2 in x)
    ----------------
    (let x := Int in x)
    ----------------
    Int
    ---------------- |}]

let%expect_test _ =
  run "let x := 2 in x + x";
  [%expect
    {|
    (let x := 2 in (x + x))
    ----------------
    (let x := Int in (x + x))
    ----------------
    (Int + Int)
    ----------------
    Int
    ---------------- |}]

let%expect_test _ =
  run "let x := 2 in x + y";
  [%expect
    {|
    (let x := 2 in (x + y))
    ----------------
    (let x := Int in (x + y))
    ----------------
    (Int + y)
    ----------------
    (Int + (Error: Unbound var y))
    ----------------
    (Error: expected (Error: Unbound var y), got Int)
    ---------------- |}]

let%expect_test _ =
  run "if true then true else false";
  [%expect
    {|
      (if true then true else false)
      ----------------
      (if Bool then true else false)
      ----------------
      (if Bool then Bool else false)
      ----------------
      (if Bool then Bool else Bool)
      ----------------
      Bool
      ---------------- |}]

let%expect_test _ =
  run "let x := 2 in (if ((x + x) = 4) then x else 2)";
  [%expect
    {|
    (let x := 2 in (if ((x + x) = 4) then x else 2))
    ----------------
    (let x := Int in (if ((x + x) = 4) then x else 2))
    ----------------
    (if ((Int + Int) = 4) then Int else 2)
    ----------------
    (if (Int = 4) then Int else 2)
    ----------------
    (if (Int = Int) then Int else 2)
    ----------------
    (if Bool then Int else 2)
    ----------------
    (if Bool then Int else Int)
    ----------------
    Int
    ---------------- |}]

let%expect_test "neg" =
  run "3 = true";
  [%expect
    {|
    (3 = true)
    ----------------
    (Int = true)
    ----------------
    (Int = Bool)
    ----------------
    (Error: expected Bool, got Int)
    ---------------- |}]

let%expect_test "neg" =
  run "true = true";
  [%expect
    {|
    (true = true)
    ----------------
    (Bool = true)
    ----------------
    (Bool = Bool)
    ----------------
    (Error: expected Bool, got Int)
    ---------------- |}]

let%expect_test "pos" =
  run "2 = 5";
  [%expect
    {|
    (2 = 5)
    ----------------
    (Int = 5)
    ----------------
    (Int = Int)
    ----------------
    Bool
    ---------------- |}]

let%expect_test "neg" =
  run "if 3 then true else true";
  [%expect
    {|
    (if 3 then true else true)
    ----------------
    (if Int then true else true)
    ----------------
    (if Int then Bool else true)
    ----------------
    (if Int then Bool else Bool)
    ----------------
    (Error: expected Bool, got Int)
    ---------------- |}]

let%expect_test "neg" =
  run "2 + true";
  [%expect
    {|
    (2 + true)
    ----------------
    (Int + true)
    ----------------
    (Int + Bool)
    ----------------
    (Error: expected Bool, got Int)
    ---------------- |}]

let%expect_test "neg" =
  run "true + 2";
  [%expect
    {|
    (true + 2)
    ----------------
    (Bool + 2)
    ----------------
    (Bool + Int)
    ----------------
    (Error: expected Bool, got Int)
    ---------------- |}]

let%expect_test "neg" =
  run "2 true";
  [%expect
    {|
    (2 true)
    ----------------
    (Int true)
    ----------------
    (Int Bool)
    ----------------
    (Error: expected fun type, got Int)
    ---------------- |}]
