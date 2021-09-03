open Etch.Node

let rec gather = function
  | Res _ as res -> [ res ]
  | res -> res :: gather (Etch.work res)

let print_step step =
  let step_str = show_expr step in
  print_endline step_str;
  print_endline "----------------"

(* let example = Int 2 *)
(* let example = Let("x", Int 2, Var"x") *)
(* let example = Let ("x", Int 2, Plus (Var "x", Var "x")) *)

(* let example = Let("x", Int 2, Plus (Var"x", Var"y")) *)
let example =
  Let ("x", Int 2, If (Eq (Plus (Var "x", Var "x"), Int 4), Var "x", Int 2))

let _ = List.iter print_step (gather @@ Etch.WellFormed.well_formed example)
