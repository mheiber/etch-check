
type ty =
    | FunTy of ty * ty
    | IntTy
    | BoolTy
    | ErrTy of string

and expr =
    | Var of string
    | Fun of string * ty * expr
    | Let of string * expr * expr
    | App of expr * expr
    | Int of int
    | Bool of bool
    | If of expr * expr * expr
    | Sugar of sugar
    | Res of ty
and sugar =
    | Plus of expr * expr
    | Eq of expr * expr


let rec show_ty = function
    | FunTy(t1, t2) ->
            Printf.sprintf "(%s -> %s)" (show_ty t1) (show_ty t2)
    | IntTy ->
            "Int"
    | BoolTy ->
        "Bool"
    | ErrTy msg ->
         Printf.sprintf "(Error: %s)" msg
 and show_expr = function
    | Var s ->
            s
    | Fun (p, pTy, e) ->
            Printf.sprintf "lambda %s : %s. %s" p (show_ty pTy) (show_expr e)
    | Let (x, e1, e2) ->
            Printf.sprintf "let %s = %s in %s" x (show_expr e1) (show_expr e2)
    | App (e1, e2) ->
            Printf.sprintf "%s %s" (show_expr e1) (show_expr e2)
    | Int n ->
            Printf.sprintf "%d" n
    | Bool true ->
            "true"
    | Bool false ->
            "false"
    | If (e1, e2, e3) ->
            Printf.sprintf
                "if %s then %s else %s"
                (show_expr e1) (show_expr e2) (show_expr e3)
    | Sugar sugar ->
            show_sugar sugar
    | Res ty ->
            show_ty ty
and show_sugar = function
    | Plus (e1, e2) ->
            Printf.sprintf
                "(%s + %s)"
                (show_expr e1) (show_expr e2)
    | Eq (e1, e2) ->
            Printf.sprintf
                "(%s = %s)"
                (show_expr e1) (show_expr e2)

module StrSet = Set.Make(String)


let rec sugar_map f = function
    | Plus (e1, e2) ->
            Plus ((f e1), (f e2))
    | Eq (e1, e2) ->
            Eq ((f e1), (f e2))

let rec sanity names = function
    | Fun (name, _, _)
    | Let (name, _, _) when StrSet.mem name names ->
            let msg =
                Printf.sprintf
                    "%s is already bound, you must use a new name"
                    name
            in
            (Res (ErrTy msg))
    | Fun (paramName, paramTy, body) ->
            let names = (StrSet.add paramName names) in
            let body = sanity names body in
            Fun (paramName, paramTy, body)
    | Let (name, e, body) ->
            let names = (StrSet.add name names) in
            let body = sanity names body in
            Let (name, e, body)
    | App(fe, arg) ->
            App (sanity names fe, sanity names arg)
    | If (cond, pos, neg) ->
            If ((sanity names cond), (sanity names pos), (sanity names neg))
    | Sugar sugar ->
            let sugar = sugar_map (fun e -> sanity names e) sugar in
            Sugar sugar
    | (Int _  | Bool _ | Res _ | Var _) as e -> e

(* type meta = 

let lift f = fun meta -> f meta.ty *)

let expected exp = function
    | ErrTy _ as got ->
            Res got
    | got ->
        let msg = Printf.sprintf
            "expected %s got %s"
            (show_ty exp) (show_ty got)
        in Res (ErrTy msg)

let mismatch_or exp got success_ty =
                if exp <> got then expected exp got
                else Res success_ty


let desugar = function
    | Plus (e1, e2) ->
        App(App(Res (FunTy(IntTy, FunTy(IntTy, IntTy))), e1), e2)
    | Eq (e1, e2) ->
        App(App(Res (FunTy(IntTy, FunTy(IntTy, BoolTy))), e1), e2)

let rec subst ty name body =
    let s = subst ty name in
    match body with
        | Var x when x = name ->
                Res ty
        | Fun (x, ty, body) ->
                Fun (x, ty, s body)
        | Let (x, e, body) ->
                Let (x, s e, s body)
        | App (fn, arg) ->
                App (s fn, s arg)
        | If (e1, e2, e3) ->
            If (s e1, s e2, s e3)
        | Sugar sugar ->
                Sugar (sugar_map s sugar)
        | Int _ | Bool _ | Res _ | Var _ -> body


let rec work: expr -> expr = function
        | Fun (x, pTy, Res resTy) ->
                Res (FunTy (pTy, resTy))
        | Fun (x, ty, body) ->
                (* TODO: avoid repeating substitution*)
                let body = subst ty x body in
                Fun (x, ty, work body)
        | Let (name, (Res ty), body) ->
                subst ty name body
        | Let (name, e, body) ->
                Let (name, work e, body)
        | App ((Res (FunTy (pTy, resTy))), Res argTy) ->
                mismatch_or pTy argTy resTy
        | App (Res ft, arg) ->
                App (Res ft, work arg)
        | App (e_fun, arg) ->
                App (work e_fun, arg)
        | Int n ->
                Res IntTy
        | Bool _ ->
                Res BoolTy
        | Var x ->
                let msg = Printf.sprintf "Unbound var %s" x in
                Res (ErrTy msg)
        | If (Res BoolTy, Res pos, Res neg) ->
                mismatch_or pos neg neg
        | If (Res ty, Res pos, Res neg) ->
                expected BoolTy ty
        | If (Res cond, Res pos, neg) ->
                If (Res cond, Res pos, work neg)
        | If (Res cond, pos, neg) ->
            If (Res cond, work pos, neg)
        | If (cond, pos, neg) ->
                If (work cond, pos, neg)
        | Sugar sugar ->
                desugar sugar
        | (Res _)as r -> r


let step expr = work @@ sanity StrSet.empty expr


let rec gather = function
    | (Res _) as res ->
            (* print_endline @@ show_expr res; *)
            res :: []
    | res ->
            (* print_endline @@ show_expr res; *)
            res :: gather (step res)

let print_step step =
    let step_str = show_expr step in
    print_endline step_str;
    print_endline "----------------"

(* let example = Int 2 *)
(* let example = Let("x", Int 2, Var"x") *)
(* let example = Let("x", Int 2, Sugar (Plus (Var"x", Var"x"))) *)
(* let example = Let("x", Int 2, Sugar (Plus (Var"x", Var"y"))) *)
let example = Let("x", Int 2,
    If (
        Sugar(Eq(
            (Sugar (Plus (Var"x", Var"x"))),
            Int 4
        )),
        Var"x",
        Int 2
    )
)
let _ = List.iter print_step (gather example)

