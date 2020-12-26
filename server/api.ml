open Jtable

open Lwt.Syntax
open Lwt_react

let events _status history = Event.events_to_string history
let status status _history = Status.to_json status
let history _status history = Event.events_to_string (List.rev history)

let data =
  [ "/api/events" , events
  ; "/api/history", history
  ; "/api/status" , status ]

let event_stream params _status event history =
  let hist = S.value history in
  let id = match params with
  | [] -> Event.latest_id hist
  | [id] -> int_of_string id
  | _ -> assert false in
  match Event.take_later_than id hist with
  | [] -> let+ ev = E.next event in Event.events_to_string [ev]
  | events -> Lwt.return (Event.events_to_string events)

let status_stream params status _event _history =
  let s = S.value status in
  let h = Status.hash s in
  let hash = match params with
  | [] -> h
  | [hash] -> int_of_string hash
  | _ -> assert false in
  match h = hash with
  | true -> Lwt.map Status.to_json (E.next (S.changes status))
  | false -> Lwt.return (Status.to_json s)

let streams =
  [ "/api/event-stream"       , event_stream , []
  ; "/api/event-stream/:id"   , event_stream , ["id"]
  ; "/api/status-stream"      , status_stream, []
  ; "/api/status-stream/:hash", status_stream, ["hash"]]


let post =
  [ "/api/post/context", `Context
  ; "/api/post/source", `Source ]
