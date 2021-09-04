open Node
module Node = Node
module WellFormed = WellFormed

let expected exp = function
  | ErrTy _ as got -> Res got
  | got ->
      let msg =
        Printf.sprintf "expected %s, got %s" (show_ty exp) (show_ty got)
      in
      Res (ErrTy msg)

let expected_fun = function
  | ErrTy _ as got -> Res got
  | got ->
      let msg = Printf.sprintf "expected fun type, got %s" (show_ty got) in
      Res (ErrTy msg)

let mismatch_or exp got success_ty =
  if exp <> got then expected exp got else Res success_ty

let rec subst ty name body =
  let s = subst ty name in
  match body with
  | Var x when x = name -> Res ty
  | Fun (x, ty, body) -> Fun (x, ty, s body)
  | Let (x, e, body) -> Let (x, s e, s body)
  | App (fn, arg) -> App (s fn, s arg)
  | If (e1, e2, e3) -> If (s e1, s e2, s e3)
  | Plus (e1, e2) -> Plus (s e1, s e2)
  | Eq (e1, e2) -> Eq (s e1, s e2)
  | Int _ | Bool _ | Res _ | Var _ -> body

let rec work expr =
  let work2 e1 e2 =
    match e1 with Res _ -> (e1, work e2) | _ -> (work e1, e2)
  in
  let work3 e1 e2 e3 =
    match (e1, e2) with
    | Res _, Res _ -> (e1, e2, work e3)
    | _ ->
        let e1, e2 = work2 e1 e2 in
        (e1, e2, e3)
  in
  match expr with
  | Fun (_, pTy, Res resTy) -> Res (FunTy (pTy, resTy))
  | Fun (None, ty, body) -> Fun (None, ty, work body)
  | Fun (Some x, ty, body) ->
      let body = subst ty x body in
      Fun (None, ty, body)
  | Let (name, Res ty, body) -> subst ty name body
  | Let (name, e, body) -> Let (name, work e, body)
  | App (Res (FunTy (pTy, resTy)), Res argTy) -> mismatch_or pTy argTy resTy
  | App (Res non_ft, Res _) -> expected_fun non_ft
  | App (f, arg) ->
      let f, arg = work2 f arg in
      App (f, arg)
  | Int _ -> Res IntTy
  | Bool _ -> Res BoolTy
  | Var x ->
      let msg = Printf.sprintf "Unbound var %s" x in
      Res (ErrTy msg)
  | If (Res BoolTy, Res pos, Res neg) -> mismatch_or pos neg neg
  | If (Res condTy, Res _, Res _) -> expected BoolTy condTy
  | If (cond, e1, e2) ->
      let cond, e1, e2 = work3 cond e1 e2 in
      If (cond, e1, e2)
  | Plus (Res t1, Res t2) ->
      if t1 <> IntTy then expected t1 IntTy
      else if t2 <> IntTy then expected t2 IntTy
      else Res IntTy
  | Plus (e1, e2) ->
      let e1, e2 = work2 e1 e2 in
      Plus (e1, e2)
  | Eq (Res t1, Res t2) ->
      if t1 <> IntTy then expected t1 IntTy
      else if t2 <> IntTy then expected t2 IntTy
      else Res BoolTy
  | Eq (e1, e2) ->
      let e1, e2 = work2 e1 e2 in
      Eq (e1, e2)
  | Res _ as r -> r
