
type loss = (string * float) list
  [@@deriving yojson]

type epoch = {
    number : int
  ; trainloss : loss
  ; testloss : loss
  ; quality : float
  ; capacity : float
  ; size : int
  ; ctxid : int
  ; era : int
} [@@deriving yojson] 

type era = {
    number : int
  ; epoch : int
  ; metrics : (string * float) list 
} [@@deriving yojson]

type contestreq = {
    reqid : int
} [@@deriving yojson]

type datareq = {
    reqid : int
  ; ctxid : int
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
  ; ip : string
  ; port : int
  ; disconnect : bool
} [@@deriving yojson]

type body =
  | Era        of era
  | Epoch      of epoch
  | Data       of data
  | Datareq    of datareq
  | Contest    of Contest.t
  | Contestreq of contestreq
  | Client     of client
  | Model      of Model.t
  | Context    of Context.t (* source of the context change *)
  [@@deriving yojson]

type time =
  { year   : int
  ; month  : int
  ; day    : int
  ; hour   : int
  ; minute : int
  ; second : int }
  [@@deriving yojson]

let time_to_string time =
  Printf.sprintf "%02d:%02d" time.hour time.minute

let datetime_to_string time =
  Printf.sprintf "%d-%02d-%02d, %02d:%02d:%02d"
    time.year time.month time.day time.hour time.minute time.second

type t = 
  { id : int
  ; time : time
  ; body : body }
  [@@deriving yojson]

let event ?time ?(id=(-1)) body =
  let default = {year = 0; month = 1; day = 1; hour = 1; minute = 1; second = 1} in
  let time = Option.value time ~default in
  {time; id; body}

let parse str =
  try Yojson.Safe.from_string str |> of_yojson with
  | Yojson.Json_error msg -> Error msg

let parse_events str =
  let f s =
    Yojson.Safe.from_string s
    |> of_yojson
    |> Result.get_ok
  in
  let lines = String.split_on_char '\n' str in
  try Ok (List.filter (fun l -> String.trim l <> "") lines |> List.map f)
  with exn ->
    let exs = Printexc.to_string exn in
    Error (Printf.sprintf "could not parse event list: %s" exs) 

let events_to_string events =
  let len = (List.length events) * 200 in
  let buf = Buffer.create len in
  let add_sep () = Buffer.add_char buf '\n' in
  let add_event ev =
    let str = to_yojson ev |> Yojson.Safe.to_string in
    Buffer.add_string buf str
  in
  match events with
  | [] -> ""
  | h :: tl ->
    add_event h;
    List.iter (fun ev -> add_sep (); add_event ev) tl;
    Buffer.contents buf
  
let to_json ev =
  to_yojson ev |> Yojson.Safe.to_string

let time ev = ev.time

let info ev =
  ev.body |> body_to_yojson |> Yojson.Safe.pretty_to_string

let summary ev = let open Printf in match ev.body with
  | Era era       -> sprintf "era %d started" era.number
  | Epoch ep      -> sprintf "epoch %d ended" ep.number
  | Data _        -> sprintf "dataset obtained"
  | Datareq dr    -> sprintf "data request %d ready" dr.reqid
  | Contest _     -> sprintf "contest results obtained"
  | Contestreq cr -> sprintf "contest request %d ready" cr.reqid
  | Context _     -> sprintf "context update"
  | Model m       -> sprintf "model '%s' initialized" m.Model.name
  | Client c -> match c.disconnect with
    | true -> sprintf "client logoff '%s'" c.name
    | false -> sprintf "client login '%s'" c.name

let has_id id ev = (ev.id = id)
let latest_id = function [] -> -1 | h :: _ -> h.id

let take_later_than id history =
  let rec f acc = function
  | [] -> acc
  | h :: t -> if h.id = id then acc else f (h :: acc) t in
  List.rev (f [] history)

let latest = function
  | [] -> None
  | h :: _ -> Some h

let rev_history_diff lnew lold =
  let rec take acc a = function
  | [] -> []
  | h :: tl -> if h = a then acc else take (h :: acc) a tl
  in
  match lold with
  | [] -> lnew
  | h :: _ -> take [] h lnew
  


