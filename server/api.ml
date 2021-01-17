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

let event_stream params _status history =
  let update = E.stamp (S.changes history) () |> E.to_stream in
  let id = match params with
  | [] -> Event.latest_id (S.value history)
  | [id] -> int_of_string id
  | _ -> assert false in
  let rec return_events () =
    match Event.take_later_than id (S.value history) with
    | [] ->
      (* TODO: should understand why the sleep is necessary here;
         otherwise E.next / Lwt_stream.next refuse to return sometimes *)
      let* () = Lwt.pick [Lwt_stream.next update; Lwt_unix.sleep 60.] in
      return_events () 
    | events -> Lwt.return (Event.events_to_string events)
  in
  return_events ()

let status_stream params status _history =
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
  [ "/api/stream/events"       , event_stream , []
  ; "/api/stream/events/:id"   , event_stream , ["id"]
  ; "/api/stream/status"       , status_stream, []
  ; "/api/stream/status/:hash" , status_stream, ["hash"]]


let post =
  [ "/api/post/context", `Context
  ; "/api/post/source", `Source ]
