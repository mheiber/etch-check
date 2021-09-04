open Node
module Node = Node

let rec drive = function
  | Res _ as res -> [ res ]
  | res -> res :: drive (Work.work res)

let check expr = drive @@ WellFormed.well_formed expr

let parse lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | Lexer.SyntaxError msg -> Res (ErrTy msg)
  | Parser.Error -> Res (ErrTy "syntax error")
