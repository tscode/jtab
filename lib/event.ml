
type loss = {
    value : float
  ; policy : float
  ; feature : float
  ; total : float
} [@@deriving yojson]

type epoch = {
    number : int
  ; trainloss : loss
  ; testloss : loss
  ; quality : float
  ; size : int
  ; ctxid : int
  ; era : int
} [@@deriving yojson] 

type era = {
    name : string
  ; number : int
  ; epoch : int
} [@@deriving yojson]

type contestreq = {
    reqid : int
} [@@deriving yojson]

type contest = {
    client : string
  ; names : string list
  ; matches : int
  ; results : int list list
  ; elo : float list
  ; draw : float
  ; sadv : float
} [@@deriving yojson]

type datareq = {
    reqid : int
  ; ctxid : int
  ; era : string
} [@@deriving yojson]

type data = {
    reqid : int
  ; client : string
  ; states : int
  ; playings : int
  ; mb : float
  ; time : float
} [@@deriving yojson]

type client = {
    name : string
  ; disconnect : bool
} [@@deriving yojson]

type body =
    Era of era
  | Epoch of epoch
  | Data of data
  | Datareq of datareq
  | Contest of contest
  | Contestreq of contestreq
  | Client of client
  | Context of Context.t (* source of the context change *)
  [@@deriving yojson]

type time =
  { year : int
  ; month : int
  ; day : int
  ; hour : int
  ; second : int } [@@deriving yojson]

let time ?(year=2019) ?(month=12) ?(day=7) ?(hour=13) ?(second=55) () =
  {year; month; day; hour; second}

let time_to_string time =
  Printf.sprintf "%02d:%02d" time.hour time.second

let datetime_to_string time =
  Printf.sprintf "%d-%02d-%02d, %02d:%02d"
    time.year time.month time.day time.hour time.second

type t = 
  { id : int
  ; time : time
  ; body : body }
  [@@deriving yojson]

type event_list = t list [@@deriving yojson]

let event ?(time=time ()) ?(id=(-1)) body = {time; id; body}

let parse str =
  Yojson.Safe.from_string str |> of_yojson

let parse_events strs =
  let f str =
    Yojson.Safe.from_string str
    |> of_yojson
    |> Result.get_ok
  in
  try Ok (List.map f strs)
  with exn ->
    let exs = Printexc.to_string exn in
    Error (Printf.sprintf "could not parse event list: %s" exs) 
  
let parse_event_list str =
  Yojson.Safe.from_string str |> event_list_of_yojson

let to_json ev =
  to_yojson ev |> Yojson.Safe.to_string

let event_list_to_json evs =
  event_list_to_yojson evs |> Yojson.Safe.to_string

let time ev = ev.time

let info ev = ev.body |> body_to_yojson |> Yojson.Safe.pretty_to_string

let summary ev = let open Printf in match ev.body with
  | Era era       -> sprintf "Era %d ended" era.number
  | Epoch ep      -> sprintf "Epoch %d ended" ep.number
  | Data _        -> sprintf "Dataset received"
  | Datareq dr    -> sprintf "Data request %d submitted" dr.reqid
  | Contest _     -> sprintf "Contest results received"
  | Contestreq cr -> sprintf "Contest request %d submitted" cr.reqid
  | Context _     -> sprintf "Context update"
  | Client c -> match c.disconnect with
    | true -> sprintf "Logoff: %s" c.name
    | false -> sprintf "Login: %s" c.name

