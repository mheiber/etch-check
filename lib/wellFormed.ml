open Node
module StrSet = Set.Make (String)

let well_formed expr =
  let rec wf names = function
    | (Fun (Some name, _, _) | Let (name, _, _)) when StrSet.mem name names ->
        let msg =
          Printf.sprintf "%s is already bound, you must use a new name" name
        in
        Res (ErrTy msg)
    | Fun (None, _pTy, _body) ->
        failwith
          "unreachable: the surface language should not allow funs with \
           unnamed params"
    | Fun (Some paramName, paramTy, body) ->
        let names = StrSet.add paramName names in
        let body = wf names body in
        Fun (Some paramName, paramTy, body)
    | Let (name, e, body) ->
        let names = StrSet.add name names in
        let body = wf names body in
        Let (name, e, body)
    | App (fe, arg) -> App (wf names fe, wf names arg)
    | If (cond, pos, neg) -> If (wf names cond, wf names pos, wf names neg)
    | Plus (e1, e2) -> Plus (wf names e1, wf names e2)
    | Eq (e1, e2) -> Eq (wf names e1, wf names e2)
    | (Int _ | Bool _ | Res _ | Var _) as e -> e
  in
  wf StrSet.empty expr
