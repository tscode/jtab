

let version = "v0.1"

open Jtable
open Printf

open Lwt.Syntax
open Lwt.Infix
open Lwt_react

let log msg = print_endline ("<jtab> " ^ msg)
let log_err msg = prerr_endline ("<jtab> error: " ^ msg)
let log_warn msg = prerr_endline ("<jtab> warning: " ^ msg)

(* Communication with the train server / load event files *)

let read_history_file path =
  let rec parse_all acc ic =
    match Event.parse (input_line ic) with
    | Ok ev -> parse_all (ev :: acc) ic
    | Error msg -> close_in_noerr ic; Error msg
    | exception _ -> close_in_noerr ic; Ok acc
  in
  try parse_all [] (open_in path) with
  | _ -> Error (sprintf "cannot open file '%s'" path)

let write_history_file events path =
  match open_out path with
  | exception _ -> Some (sprintf "cannot open file '%s'" path)
  | oc ->
    let hist = Event.events_to_string events in
    output_string oc hist; close_out_noerr oc;
    None

let create_data_dir dir =
  match Unix.mkdir dir 0o777 with
  | () -> log (sprintf "created data directory '%s'" dir)
  | exception _ -> ()

let resolve_data_path dir path =
  Filename.(concat dir (basename path))

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
  let* event = Lwt_io.read_line_opt ic in
  match event with
  | None ->
    let msg = "listening to events failed: connection lost" in
    Lwt.return (`Connection_lost msg)
  | Some str ->
  match Event.parse str with
  | Error msg -> Lwt.return (`Fatal_error msg)
  | Ok ev -> push ev; receive_events push ic

let rec send_context ctx oc =
  let* context = E.next ctx in
  let handle_exn _ =
    let msg = "sending context failed: connection lost" in
    Lwt.return (`Connection_lost msg)
  in
  let send () =
    let* () = Lwt_io.write oc (Context.to_json context) in
    let* () = Lwt_unix.sleep 0.5 in  (* TODO: why do we have an arbitrary sleep here? *)
    send_context ctx oc
  in
  Lwt.catch send handle_exn

let connect_source delay status set_status history set_history ctx =
  let close, send_close = E.create () in
  let error source msg =
    log_err msg; set_status (`Error (source, msg))
  in
  let reconnect source =
    log (sprintf "trying again in %g seconds..." delay);
    let+ res = Lwt.pick
      [ Lwt.map (Fun.const `Source_changed) (E.next close)
      ; Lwt.map (Fun.const `Delay_over) (Lwt_unix.sleep delay) ]
    in
    match res with
    | `Source_changed -> log "aborted retry since source has been changed"
    | `Delay_over     -> set_status (`Connecting source)
  in
  let push ev = set_history (ev :: S.value history) in
  let connect_tcp source ip port =
    let* c = open_connection ip port in match c with
    | Error msg -> error source msg; reconnect source
    | Ok (ic, oc) ->
      set_history [];
      log (sprintf "connected to %s:%d" ip port);
      set_status (`Ok source);
      let msg = "connection to %s:%d closed" in
      let* res = Lwt.pick
        [ Lwt.map (Fun.const (`Source_changed msg)) (E.next close)
        ; receive_events push ic 
        ; send_context ctx oc ]
      in
      let* () = Lwt_io.close ic <&> Lwt_io.close oc in
      match res with
      | `Source_changed msg  -> Lwt.return (log_err msg)
      | `Fatal_error msg     -> Lwt.return (error source msg)
      | `Connection_lost msg -> error source msg; reconnect source
  in
  let on_source_change = function
    | `Ok _ | `Error _ -> Lwt.return ()
    | `Connecting source ->
    send_close ();
    set_history [];
    match source with
    | `History _ -> assert false (* a history should never arrive here *)
    | `Debug -> log "entering debug mode" |> Lwt.return
    | `TCP (ip, port) -> connect_tcp source ip port 
    | `File path ->
    match read_history_file path with
    | Error msg ->
      log_err msg;
      log_warn "no events will be exposed";
      set_status (`Error (source, msg)) |> Lwt.return
    | Ok events ->
      log (sprintf "loading history file '%s'" path);
      set_history events;
      set_status (`Ok (source)) |> Lwt.return
  in
  (* Use streams instead of E.map here since setting signal during
   * react update step might lead to problems. This seems to work fine *)
  let stream = E.to_stream (S.changes status) in
  let* () = on_source_change (S.value status) in
  Lwt_stream.iter_p on_source_change stream

let login _ _ _ = Ok ""


(* Web server *)

open Opium

let register_api_data status history (uri, f) =
  App.get uri (fun _req -> 
    let str = f (S.value status) (S.value history) in
    Response.(of_plain_text str |> set_content_type "text/plain")
    |> Lwt.return)

let register_api_stream status event history (uri, f, params) =
  App.get uri (fun req ->
    let params = List.map (Router.param req) params in
    let+ str = f params status event history in
    Response.(of_plain_text str |> set_content_type "text/event-stream"))

let register_api_post handle (uri, kind) =
  App.post uri (fun req ->
    let+ body = Request.to_plain_text req in
    match handle body kind with
    | Ok response -> response
    | Error msg -> log_err msg;
      Response.of_plain_text msg
      |> Response.set_content_type "text/plain"
      |> Response.set_status `Bad_request)

let register_static (uri, path, mime) =
  App.get uri (fun _req ->
    let str = Static.read path in
    Response.(of_plain_text str |> set_content_type mime) |> Lwt.return)

let register_webpage (uri, f) =
  App.get uri (fun _req -> 
    let page = f version in
    Response.(of_plain_text page |> set_content_type "text/html") |> Lwt.return)

let serve port handle_msg status history =
  let event = E.fmap Event.latest (S.changes history) in
  let server = App.empty
  |> App.port port
  |> List.fold_right (register_api_data status history) Api.data
  |> List.fold_right (register_api_stream status event history) Api.streams
  |> List.fold_right (register_api_post handle_msg) Api.post
  |> List.fold_right (register_static) Static.data
  |> List.fold_right (register_webpage) Webpage.uris
  |> App.start in
  log (Printf.sprintf "webserver listens on 0.0.0.0:%d" port);
  server

(* Client posts new event source *)
let handle_source resolve_path send_status body =
  match Jtable.Status.source_of_json body with
  | Error msg -> Error (sprintf "failed to parse source: %s" msg)
  | Ok src ->
  let source = match src with
    | `TCP _ | `Debug -> Ok src
    | `File file -> Ok (`File (resolve_path file))
    | `History (name, str) ->
    log (sprintf "received history file '%s' as file upload" name);
    match Event.parse_events str with
    | Error err -> Error err
    | Ok events ->
    let path = resolve_path name in
    log (sprintf "saving history file '%s' as '%s'" name path);
    match write_history_file events path with
    | None -> Ok (`File path)
    | Some msg -> Error msg
  in
  match source with
  | Error msg -> Error msg
  | Ok src ->
    send_status (`Connecting src);
    Response.make ()
    |> Response.set_location "/"
    |> Response.set_status (Status.of_code 301)
    |> Result.ok

(* Client posts new context. Only meaningful if currently connected
 * to jtac trainer *)
let handle_context send_ctx status body =
  match Jtable.Context.parse body with
  | Error msg -> Error ("failed to parse context: " ^ msg)
  | Ok ctx ->
  match S.value status with
  | `Ok (`TCP _) ->
    send_ctx ctx;
    Response.make ()
    |> Response.set_status `No_content
    |> Result.ok
  | _ ->
    let msg = "failed to submit context: no active connection to jtac server"
    in Error msg

(* Handle messages from the client, i.e., from the javascript side *)
let handle_post resolve_path send_ctx status send_status body src =
  match src with
  | `Source -> handle_source resolve_path send_status body
  | `Context -> handle_context send_ctx status body

(* Main program *)

let main delay debug data port source =
  let src = if debug then `Debug else source in
  let status, send_status = S.create (`Connecting src) in
  let history, send_history = S.create [] in
  let ctx, send_ctx = E.create () in
  let webserver =
    let resolve = resolve_data_path data in
    let handle_msg = handle_post resolve send_ctx status send_status in
    serve port handle_msg status history
  in
  let events =
    connect_source delay status send_status history send_history ctx
  in
  if debug then send_history (Debug.test_events 100); 
  Debug.log_status_events status history;
  create_data_dir data;
  Lwt_main.run @@ Lwt.both webserver events

open Cmdliner
open Jtable

let delay =
  let doc = "Wait DELAY seconds before trying to reconnect to the jtac
  server after an unsuccessful connection attempt." in
  Arg.(value & opt float 30. & info ["d"; "delay"] ~docv:"DELAY" ~doc)

(*
let host =
  let doc = "Ip address on which the jtab website is served." in
  let sym = ["h"; "host"; "ip"] in
  Arg.(value & opt string "127.0.0.1" & info sym ~docv:"IP" ~doc)
*)

let port =
  let doc = "Port on which the jtab website is served." in
  Arg.(value & opt int 7790 & info ["p"; "port"] ~docv:"PORT" ~doc)

let data =
  let doc = "Folder where files uploaded by clients are stored." in
  let docv = "DATA-FOLDER" in
  Arg.(value & opt string "data" & info ["data"] ~docv ~doc)

let source =
  let doc = "Event source. Can be a local file (syntax 'file://[filename]') or
  a tcp socket address (syntax 'tcp://[host]:[port]'). If the protocol is not
  specified, a heuristic is used to decide." in
  let src =
    let parse s = match Status.parse_source s with
    | Some v -> Ok v
    | None -> Error (`Msg ("cannot parse source " ^ s)) in
    let print ppf s = Format.fprintf ppf "%s" (Status.source_to_string s) in
    Arg.conv (parse, print)
  in
  let def = `TCP ("127.0.0.1", 7788) in
  Arg.(value & pos 0 src def & info [] ~docv:"SOURCE" ~doc)

let debug =
  let doc = "Start jtab in a debug mode with dummy events preloaded" in
  Arg.(value & flag & info ["debug"] ~doc)

let jtab_t = Term.(const main $ delay $ debug $ data $ port $ source)

let info =
  let doc = "monitor jtac training progress" in
  let man = [
    `S Manpage.s_bugs;
    `P "Please report any issues you experience at
    https://github.com/tscode/jtab/issues." ]
  in
  Term.info "jtab" ~version ~doc ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (jtab_t, info)




