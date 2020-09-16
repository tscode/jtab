
let version = "v0.1"

open Jtable

open Lwt.Syntax
open Lwt_react

let log msg = print_endline ("<jtab> " ^ msg)
let log_err msg = prerr_endline ("<jtab> error: " ^ msg)
let log_warn msg = prerr_endline ("<jtab> warning: " ^ msg)

(* Communication with the train server / load event files *)

let read_history_file path =
  let rec parse_all acc ic =
    match Event.parse (input_line ic) with
    | Ok ev -> parse_all (ev :: acc) ic
    | Error msg -> Error msg
    | exception _ -> Ok acc
  in
  try parse_all [] (open_in path) with
  | _ -> Error ("cannot open file " ^ path)

let open_connection ip port =
  match Unix.inet_addr_of_string ip with
  | exception _ -> Error (ip ^ " is not a valid ip address") |> Lwt.return
  | ipaddr ->
    let addr = Lwt_unix.ADDR_INET (ipaddr, port) in
    let handle_exn _ =
      let err =
        Printf.sprintf "connecting to jtac instance %s:%d failed" ip port in
      Error err |> Lwt.return
    in
    let connect () = Lwt.map Result.ok (Lwt_io.open_connection addr) in
    Lwt.catch connect handle_exn 

let rec receive_events push ic =
  let* event = Lwt_io.read_line_opt ic in
  match event with
  | None -> Lwt.return (Ok "reading event failed: connection lost")
  | Some str ->
  match Event.parse str with
  | Error msg -> Lwt.return (Error msg)
  | Ok ev -> push ev; receive_events push ic

let rec send_context ctx oc =
  let* context = E.next ctx in
  let handle_exn _ =
    Lwt.return (Ok "sending context failed: connection lost")
  in
  let send () =
    let* () = Lwt_io.write oc (Context.to_json context) in
    let* () = Lwt_unix.sleep 0.5 in 
    send_context ctx oc
  in
  Lwt.catch send handle_exn

let reconnect delay send source =
  log (Printf.sprintf "trying again in %g seconds..." delay);
  let+ () = Lwt_unix.sleep delay in
  send (`Connecting source)

let connected send source ip port =
  log (Printf.sprintf "connected to %s:%d" ip port);
  send (`Ok source)

let error send source msg =
  log_err msg;
  send (`Error (source, msg))

let connect_source delay status set_status history set_history ctx =
  let close, send_close = E.create () in
  let push ev = set_history (ev :: S.value history) in
  let connect_tcp source ip port =
    let* c = open_connection ip port in match c with
    | Error msg ->
      error set_status source msg;
      reconnect delay set_status source
    | Ok (ic, oc) ->
      set_history [];
      connected set_status source ip port;
      let c = let+ () = E.next close in Ok "connection closed" in
      let r = receive_events push ic in
      let s = send_context ctx oc in
      let* ans = Lwt.pick [ c; r; s ] in
      let* () = Lwt_io.close ic in
      let* () = Lwt_io.close oc in
      match ans with
      | Error msg -> Lwt.return (error set_status source msg)
      | Ok msg ->
        error set_status source msg;
        reconnect delay set_status source
  in
  let on_source_change = function
    | `Ok _ | `Error _ -> Lwt.return ()
    | `Connecting source ->
    match source with
    | `Unknown -> log "entering debug mode" |> Lwt.return
    | `TCP (ip, port) -> send_close (); connect_tcp source ip port 
    | `File path -> match read_history_file path with
      | Error msg ->
        log_err msg;
        log_warn "no events will be exposed";
        set_status (`Error (source, msg)) |> Lwt.return
      | Ok events ->
        log ("loaded history file " ^ path);
        set_history events;
        set_status (`Ok (source)) |> Lwt.return
  in
  (* Use streams instead of E.map here since setting signal during
   * react update step might lead to problems. This seems to work fine *)
  let stream = E.to_stream (S.changes status) in
  let* () = on_source_change (S.value status) in
  Lwt_stream.iter_s on_source_change stream

let register _ _ _ = Ok ""


(* Web server *)

open Opium.Std

let headers mime =
  Cohttp.Header.init_with "content-type" mime

let register_stream status event history (uri, fstream, params) =
  get uri (fun req ->
    let params = List.map (App.param req) params in
    let headers = headers "text/event-stream" in
    let* str = fstream params status event history in
    `String str |> respond' ~headers)

let register_static status history (uri, fpage) =
  get uri (fun _req -> 
    let headers = headers "text/plain" in
    `String (fpage status (S.value history)) |> respond' ~headers)

let register_page (uri, fpage) =
  get uri (fun _req -> 
    let headers = headers "text/html" in
    `Html (fpage version) |> respond' ~headers)

let register_file (uri, path, mime) =
  get uri (fun _req ->
    let headers = headers mime in
    `String (Web.file_content path) |> respond' ~headers)

let register_msg handle uri =
  post uri (fun req ->
    let+ body = Request.body req |> Body.to_string in
    match Result.bind (Msg.of_json body) handle with
    | Ok response -> response
    | Error msg -> log_err msg;
      let headers = headers "text/plain" in
      let body = Body.of_string msg in
      Response.create ~code:(`Code 400) ~headers ~body ())

let serve_webpage port handle_msg status history =
  let event = E.fmap Event.latest (S.changes history) in
  let server () = App.empty
  |> App.port port
  |> List.fold_right (register_stream status event history) Web.api_stream
  |> List.fold_right (register_static status history) Web.api_static
  |> List.fold_right (register_file) Web.files
  |> List.fold_right (register_page) Web.pages
  |> register_msg handle_msg Web.api_msg
  |> App.start in
  let msg = Printf.sprintf "webserver listens on 0.0.0.0:%d" port in
  log msg;
  Lwt.async server;
  fst (Lwt.wait ())

let handle_msg send_ctx status send_status = function
  | Msg.Source src ->
    send_status (`Connecting src);
    let headers = Cohttp.Header.init_with "location" "/" in
    Response.create ~code:(`Code 301) ~headers () |> Result.ok
  | Msg.Context ctx ->
    match S.value status with
    | `Ok (`TCP _) -> send_ctx ctx;
      Response.create ~code:(`Code 204) () |> Result.ok
    | _ -> Result.error "failed to submit context: no active connection"

(* Main program *)

let main delay debug port source =
  let src = if debug then `Unknown else source in
  let status, send_status = S.create (`Connecting src) in
  let history, send_history = S.create [] in
  let ctx, send_ctx = E.create () in
  let webserver =
    let handle_msg = handle_msg send_ctx status send_status in
    serve_webpage port handle_msg status history
  in
  let events =
    connect_source delay status send_status history send_history ctx
  in
  if debug then send_history (Debug.test_events 100); 
  Debug.log_status_events status history;
  Lwt_main.run @@ Lwt.pick [webserver; events]

open Cmdliner

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

let source =
  let doc = "Event source. Can be a local file (syntax 'file://[filename]') or
  a tcp socket address (syntax 'tcp://[host]:[port]'). If the protocol is not
  specified, a heuristic is used to decide." in
  let src =
    let parse s = match Msg.parse_source s with
    | Some v -> Ok v
    | None -> Error (`Msg ("cannot parse source " ^ s)) in
    let print ppf s = Format.fprintf ppf "%s" (Msg.source_to_string s) in
    Arg.conv (parse, print)
  in
  let def = `TCP ("127.0.0.1", 7788) in
  Arg.(value & pos 0 src def & info [] ~docv:"SOURCE" ~doc)

let debug =
  let doc = "Start jtab in a debug mode with dummy events preloaded" in
  Arg.(value & flag & info ["debug"] ~doc)

let jtab_t = Term.(const main $ delay $ debug $ port $ source)

let info =
  let doc = "monitor jtac training progress" in
  let man = [
    `S Manpage.s_bugs;
    `P "Please report any issues you experience at
    https://github.com/tscode/jtab/issues." ]
  in
  Term.info "jtab" ~version ~doc ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (jtab_t, info)




