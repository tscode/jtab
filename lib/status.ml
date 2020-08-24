
type t =
  { connected : bool
  ; ip : string
  ; port : int
  ; msg : string option }
  [@@deriving yojson]

let to_json st = to_yojson st |> Yojson.Safe.to_string
let of_json str = Yojson.Safe.from_string str |> of_yojson

let make ~ip ~port ?msg connected = { connected; ip; port; msg }

