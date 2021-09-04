open Etch.Node

let print_step step =
  let step_str = show_expr step in
  print_endline step_str;
  print_endline "----------------"

let check example =
  example |> Lexing.from_string |> Etch.parse |> Etch.check
  |> List.iter print_step

let%expect_test _ =
  check "3";
  [%expect {|
    3
    ----------------
    Int
    ---------------- |}]

let%expect_test _ =
  check "let x := 2 in x";
  [%expect
    {|
    (let x := 2 in x)
    ----------------
    (let x := Int in x)
    ----------------
    Int
    ---------------- |}]

let%expect_test _ =
  check "let x := 2 in x + x";
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
  check "let x := 2 in x + true";
  [%expect
    {|
    (let x := 2 in (x + true))
    ----------------
    (let x := Int in (x + true))
    ----------------
    (Int + true)
    ----------------
    (Int + Bool)
    ----------------
    (Error: expected Bool, got Int)
    ---------------- |}]

let%expect_test _ =
  check "if true then true else false";
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
  check "let x := 2 in (if ((x + x) = 4) then x else 2)";
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
  check "3 = true";
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
  check "true = true";
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
  check "2 = 5";
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
  check "if 3 then true else true";
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
  check "(1 + true) + 3";
  [%expect
    {|
    ((1 + true) + 3)
    ----------------
    ((Int + true) + 3)
    ----------------
    ((Int + Bool) + 3)
    ----------------
    ((Error: expected Bool, got Int) + 3)
    ----------------
    ((Error: expected Bool, got Int) + Int)
    ----------------
    (Error: expected Bool, got Int)
    ---------------- |}]

let%expect_test "neg" =
  check "true + 2";
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
  check "2 true";
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

let%expect_test "neg" =
  check "if ((2 + 2) = 4) then (if true then true else true) else (1 = true)";
  [%expect
    {|
    (if ((2 + 2) = 4) then (if true then true else true) else (1 = true))
    ----------------
    (if ((Int + 2) = 4) then (if true then true else true) else (1 = true))
    ----------------
    (if ((Int + Int) = 4) then (if true then true else true) else (1 = true))
    ----------------
    (if (Int = 4) then (if true then true else true) else (1 = true))
    ----------------
    (if (Int = Int) then (if true then true else true) else (1 = true))
    ----------------
    (if Bool then (if true then true else true) else (1 = true))
    ----------------
    (if Bool then (if Bool then true else true) else (1 = true))
    ----------------
    (if Bool then (if Bool then Bool else true) else (1 = true))
    ----------------
    (if Bool then (if Bool then Bool else Bool) else (1 = true))
    ----------------
    (if Bool then Bool else (1 = true))
    ----------------
    (if Bool then Bool else (Int = true))
    ----------------
    (if Bool then Bool else (Int = Bool))
    ----------------
    (if Bool then Bool else (Error: expected Bool, got Int))
    ----------------
    (Error: expected Bool, got Int)
    ---------------- |}]

let%expect_test "neg" =
  check "let f := (3 + true) in f 2";
  [%expect
    {|
      ((let f := (3 + true) in f) 2)
      ----------------
      ((let f := (Int + true) in f) 2)
      ----------------
      ((let f := (Int + Bool) in f) 2)
      ----------------
      ((let f := (Error: expected Bool, got Int) in f) 2)
      ----------------
      ((Error: expected Bool, got Int) 2)
      ----------------
      ((Error: expected Bool, got Int) Int)
      ----------------
      (Error: expected Bool, got Int)
      ---------------- |}]

let%expect_test "" =
  check "(fun x : Int := 2 = x) 1";
  [%expect
    {|
    ((fun x : Int := (2 = x)) 1)
    ----------------
    ((fun _ : Int := (2 = Int)) 1)
    ----------------
    ((fun _ : Int := (Int = Int)) 1)
    ----------------
    ((fun _ : Int := Bool) 1)
    ----------------
    ((Int -> Bool) 1)
    ----------------
    ((Int -> Bool) Int)
    ----------------
    Bool
    ---------------- |}]

let%expect_test "" =
  check "(fun x : Int := fun y : Int := 2 = (x + y)) 1 2";
  [%expect
    {|
    (((fun x : Int := (fun y : Int := (2 = (x + y)))) 1) 2)
    ----------------
    (((fun _ : Int := (fun y : Int := (2 = (Int + y)))) 1) 2)
    ----------------
    (((fun _ : Int := (fun _ : Int := (2 = (Int + Int)))) 1) 2)
    ----------------
    (((fun _ : Int := (fun _ : Int := (Int = (Int + Int)))) 1) 2)
    ----------------
    (((fun _ : Int := (fun _ : Int := (Int = Int))) 1) 2)
    ----------------
    (((fun _ : Int := (fun _ : Int := Bool)) 1) 2)
    ----------------
    (((fun _ : Int := (Int -> Bool)) 1) 2)
    ----------------
    (((Int -> (Int -> Bool)) 1) 2)
    ----------------
    (((Int -> (Int -> Bool)) Int) 2)
    ----------------
    ((Int -> Bool) 2)
    ----------------
    ((Int -> Bool) Int)
    ----------------
    Bool
    ---------------- |}]
