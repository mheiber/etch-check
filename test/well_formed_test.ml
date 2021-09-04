open Etch.Node

let print_step step =
  let step_str = show_expr step in
  print_endline step_str;
  print_endline "----------------"

let run example =
  example |> Lexing.from_string |> Etch.parse |> Etch.check
  |> List.iter print_step

let%expect_test "neg" =
  run "let x := 2 in let x := 3 in x";
  [%expect
    {|
    (let x := 2 in (Error: x is already bound, you must use a new name))
    ----------------
    (let x := Int in (Error: x is already bound, you must use a new name))
    ----------------
    (Error: x is already bound, you must use a new name)
    ---------------- |}]

let%expect_test "pos" =
  run "let x := 2 in let z := 3 in x";
  [%expect
    {|
    (let x := 2 in (let z := 3 in x))
    ----------------
    (let x := Int in (let z := 3 in x))
    ----------------
    (let z := 3 in Int)
    ----------------
    (let z := Int in Int)
    ----------------
    Int
    ---------------- |}]

let%expect_test "pos" =
  run "let add2 := fun x : Int := x + 2 in let x := 3 in (add2 x)";
  [%expect
    {|
    (let add2 := (fun x : Int := (x + 2)) in (let x := 3 in (add2 x)))
    ----------------
    (let add2 := (fun _ : Int := (Int + 2)) in (let x := 3 in (add2 x)))
    ----------------
    (let add2 := (fun _ : Int := (Int + Int)) in (let x := 3 in (add2 x)))
    ----------------
    (let add2 := (fun _ : Int := Int) in (let x := 3 in (add2 x)))
    ----------------
    (let add2 := (Int -> Int) in (let x := 3 in (add2 x)))
    ----------------
    (let x := 3 in ((Int -> Int) x))
    ----------------
    (let x := Int in ((Int -> Int) x))
    ----------------
    ((Int -> Int) Int)
    ----------------
    Int
    ---------------- |}]

let%expect_test "neg" =
  run "let add2 := fun x : Int := x + 2 in let add2 := 3 in (add2 add2)";
  [%expect
    {|
    (let add2 := (fun x : Int := (x + 2)) in (Error: add2 is already bound, you must use a new name))
    ----------------
    (let add2 := (fun _ : Int := (Int + 2)) in (Error: add2 is already bound, you must use a new name))
    ----------------
    (let add2 := (fun _ : Int := (Int + Int)) in (Error: add2 is already bound, you must use a new name))
    ----------------
    (let add2 := (fun _ : Int := Int) in (Error: add2 is already bound, you must use a new name))
    ----------------
    (let add2 := (Int -> Int) in (Error: add2 is already bound, you must use a new name))
    ----------------
    (Error: add2 is already bound, you must use a new name)
    ---------------- |}]

let%expect_test "pos" =
  run {| fun x : Int := fun y : Int := true |};
  [%expect
    {|
    (fun x : Int := (fun y : Int := true))
    ----------------
    (fun _ : Int := (fun y : Int := true))
    ----------------
    (fun _ : Int := (fun _ : Int := true))
    ----------------
    (fun _ : Int := (fun _ : Int := Bool))
    ----------------
    (fun _ : Int := (Int -> Bool))
    ----------------
    (Int -> (Int -> Bool))
    ---------------- |}]

let%expect_test "neg" =
  run {| fun x : Int := fun x : Int := true |};
  [%expect
    {|
    (fun x : Int := (Error: x is already bound, you must use a new name))
    ----------------
    (Int -> (Error: x is already bound, you must use a new name))
    ---------------- |}]
