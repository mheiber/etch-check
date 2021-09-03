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

val show_ty : ty -> string

val show_expr : expr -> string
