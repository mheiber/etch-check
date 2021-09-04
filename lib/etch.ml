open Node

module Node = Node
module WellFormed = WellFormed

let rec drive = function
  | Res _ as res -> [ res ]
  | res -> res :: drive (Work.work res)

let check expr = drive @@ WellFormed.well_formed expr
