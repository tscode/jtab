
open Jtable

open Printf
open Lwt
open Lwt.Syntax
open Lwt_react

type login = {
    name : string
 ;  token : string
 ;  version : string
 ;  kind : string
} [@@deriving yojson]

let login name token version =
  { name; token; version; kind = "monitor" }

type auth = {
    accept : bool
  ; msg : string
  ; session : string
} [@@deriving yojson]

let parse_auth str =
  try Yojson.Safe.from_string str |> auth_of_yojson with
  | Yojson.Json_error msg -> Error msg

let login_to_json l =
  login_to_yojson l |> Yojson.Safe.to_string

let read_line_opt ic =
  let handle_exn = function
  | Unix.Unix_error _ -> Lwt.return_none
  | exn -> Lwt.fail exn
  in
  Lwt.catch (fun () -> Lwt_io.read_line_opt ic) handle_exn

let login_query l ic oc =
  let close msg =
    let+ () = Lwt_io.close ic <&> Lwt_io.close oc in
    Error msg
  in
  let* () = Lwt_io.write_line oc (login_to_json l) in
  let* ans = read_line_opt ic in
  match ans with
  | None -> close "connection closed unexpectedly"
  | Some str ->
  match parse_auth str with
  | Error err -> close (sprintf "login response could not be parsed: %s" err)
  | Ok auth ->
  match auth.accept with
  | false -> close (sprintf "login failed: %s" auth.msg)
  | true -> Lwt.return (Ok (auth.msg, auth.session))

let open_connection ip port =
  match Unix.inet_addr_of_string ip with
  | exception _ -> Error (ip ^ " is not a valid ip address") |> Lwt.return
  | ipaddr ->
    let addr = Lwt_unix.ADDR_INET (ipaddr, port) in
    let handle_exn _ =
      let err = sprintf "connecting to jtac instance %s:%d failed" ip port in
      Error err |> Lwt.return
    in
    let connect () = Lwt.map Result.ok (Lwt_io.open_connection addr) in
    Lwt.catch connect handle_exn 

let rec receive_events push ic =
  let* event = read_line_opt ic in
  match event with
  | None ->
    let msg = "listening to events failed: connection lost" in
    Lwt.return (`Connection_lost msg)
  | Some str ->
  match Event.parse str with
  | Ok ev -> push ev; receive_events push ic
  | Error err ->
    let msg = sprintf "listening to events failed: parsing error (%s)" err in
    Lwt.return (`Fatal_error msg)

let rec send_context ctx oc =
  let* context = E.next ctx in
  let handle_exn _ =
    let msg = "sending context failed: connection lost" in
    Lwt.return (`Connection_lost msg)
  in
  let send () =
    let ev = Event.(event (Context context)) in
    let* () = Lwt_io.write_line oc (Event.to_json ev) in
    let* () = Lwt_unix.sleep 0.5 in  (* TODO: why do we have an arbitrary sleep here? *)
    send_context ctx oc
  in
  Lwt.catch send handle_exn

let connect l delay close set_status history set_history ctx source ip port =
  let push ev = set_history (ev :: S.value history) in
  let on_error source msg =
    Log.err msg; set_status (`Error (source, msg))
  in
  let try_reconnect source =
    Log.log (sprintf "trying again in %g seconds..." delay);
    let+ res = Lwt.pick
      [ Lwt.map (Fun.const `Source_changed) (E.next close)
      ; Lwt.map (Fun.const `Delay_over) (Lwt_unix.sleep delay) ]
    in
    match res with
    | `Source_changed -> Log.log "aborted retry since source has been changed"
    | `Delay_over     -> set_status (`Connecting source)
  in
  let* c = open_connection ip port in match c with
  | Error msg -> on_error source msg; try_reconnect source
  | Ok (ic, oc) ->
    let* auth = login_query l ic oc in match auth with
    | Error err -> set_status (`Error (source, err)); Lwt.return (Log.err err)
    | Ok (msg, sess) ->
    set_history [];
    Log.log (sprintf "connected to %s:%d (session %s)" ip port sess);
    Log.log (sprintf "message from %s:%d received:\n  %s" ip port msg);
    set_status (`Ok source);
    let msg = "connection to %s:%d closed" in
    let* res = Lwt.pick
      [ Lwt.map (Fun.const (`Source_changed msg)) (E.next close)
      ; receive_events push ic 
      ; send_context ctx oc ]
    in
    let* () = Lwt_io.close ic <&> Lwt_io.close oc in
    match res with
    | `Source_changed msg  -> Lwt.return (Log.err msg)
    | `Fatal_error msg     -> Lwt.return (on_error source msg)
    | `Connection_lost msg -> on_error source msg; try_reconnect source

