open Etch.Node

let rec gather = function
  | Res _ as res -> [ res ]
  | res -> res :: gather (Etch.work res)

let print_step step =
  let step_str = show_expr step in
  print_endline step_str;
  print_endline "----------------"

let run example =
  List.iter print_step (gather @@ Etch.WellFormed.well_formed example)

let%expect_test _ =
  run @@ Int 3;
  [%expect {|
    3
    ----------------
    Int
    ---------------- |}]

let%expect_test _ =
  run @@ Let ("x", Int 2, Var "x");
  [%expect
    {|
    let x = 2 in x
    ----------------
    let x = Int in x
    ----------------
    Int
    ---------------- |}]

let%expect_test _ =
  run @@ Let ("x", Int 2, Plus (Var "x", Var "x"));
  [%expect
    {|
    let x = 2 in (x + x)
    ----------------
    let x = Int in (x + x)
    ----------------
    (Int + Int)
    ----------------
    Int
    ---------------- |}]

let%expect_test _ =
  run @@ Let ("x", Int 2, Plus (Var "x", Var "y"));
  [%expect
    {|
    let x = 2 in (x + y)
    ----------------
    let x = Int in (x + y)
    ----------------
    (Int + y)
    ----------------
    (Int + (Error: Unbound var y))
    ----------------
    (Error: expected (Error: Unbound var y), got Int)
    ---------------- |}]

let%expect_test _ =
  run
  @@ Let ("x", Int 2, If (Eq (Plus (Var "x", Var "x"), Int 4), Var "x", Int 2));
  [%expect
    {|
    let x = 2 in if ((x + x) = 4) then x else 2
    ----------------
    let x = Int in if ((x + x) = 4) then x else 2
    ----------------
    if ((Int + Int) = 4) then Int else 2
    ----------------
    if (Int = 4) then Int else 2
    ----------------
    if (Int = Int) then Int else 2
    ----------------
    if Bool then Int else 2
    ----------------
    if Bool then Int else Int
    ----------------
    Int
    ---------------- |}]
