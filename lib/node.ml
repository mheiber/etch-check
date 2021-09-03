type ty = FunTy of ty * ty | IntTy | BoolTy | ErrTy of string

and expr =
  | Var of string
  | Fun of string option * ty * expr
  | Let of string * expr * expr
  | App of expr * expr
  | Int of int
  | Bool of bool
  | If of expr * expr * expr
  | Plus of expr * expr
  | Eq of expr * expr
  | Res of ty

let rec show_ty = function
  | FunTy (t1, t2) -> Printf.sprintf "(%s -> %s)" (show_ty t1) (show_ty t2)
  | IntTy -> "Int"
  | BoolTy -> "Bool"
  | ErrTy msg -> Printf.sprintf "(Error: %s)" msg

and show_expr = function
  | Var s -> s
  | Fun (None, pTy, e) ->
      Printf.sprintf "lambda _ : %s. %s" (show_ty pTy) (show_expr e)
  | Fun (Some p, pTy, e) ->
      Printf.sprintf "lambda %s : %s. %s" p (show_ty pTy) (show_expr e)
  | Let (x, e1, e2) ->
      Printf.sprintf "let %s = %s in %s" x (show_expr e1) (show_expr e2)
  | App (e1, e2) -> Printf.sprintf "%s %s" (show_expr e1) (show_expr e2)
  | Int n -> Printf.sprintf "%d" n
  | Bool true -> "true"
  | Bool false -> "false"
  | If (e1, e2, e3) ->
      Printf.sprintf "if %s then %s else %s" (show_expr e1) (show_expr e2)
        (show_expr e3)
  | Plus (e1, e2) -> Printf.sprintf "(%s + %s)" (show_expr e1) (show_expr e2)
  | Eq (e1, e2) -> Printf.sprintf "(%s = %s)" (show_expr e1) (show_expr e2)
  | Res ty -> show_ty ty
