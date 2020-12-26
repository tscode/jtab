

type t =
  | Context of Context.t
  | Source of Status.source
  [@@deriving yojson]

let context ctx = Context ctx
let source src = Source src

let to_json msg = to_yojson msg |> Yojson.Safe.to_string
let of_json str = Yojson.Safe.from_string str |> of_yojson
