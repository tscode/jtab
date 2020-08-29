
let version = "v0.1"

let ip = "127.0.0.1"
let port = 7789
let webport = 7790

open Jtable

open Lwt.Syntax
open Lwt_react

exception ConnectionLost

(* Communication with the Jtac train server *)

let rec receive_events change_status push ic =
  let* event = Lwt_io.read_line_opt ic in
  match event with
  | None -> raise ConnectionLost
  | Some str ->
    begin match Event.parse str with
    | Error msg -> change_status ?msg:(Some msg) true
    | Ok ev -> push ev
    end;
    receive_events change_status push ic

let rec send_context ctx oc =
  let* context = Lwt_stream.next (E.to_stream ctx) in
  let* () = Lwt_io.write oc (Context.to_json context) in
  send_context ctx oc

let connect_train_server set_status push_event ctx ip port =
  let change_status ?msg v = set_status (Status.make ~ip ~port ?msg v) in
  let ipaddr = Unix.inet_addr_of_string ip in
  let addr = Lwt_unix.ADDR_INET (ipaddr, port) in
  let connect () =
    let* (ic, oc) = Lwt_io.open_connection addr in
    change_status true;
    Lwt.pick
      [ receive_events change_status push_event ic
      ; send_context ctx oc ]
  in
  let rec handle_exception exn = let open Printf in
    let msg = match exn with
      | ConnectionLost -> sprintf "Connection to %s:%d interrupted." ip port
      | _ -> sprintf "Connection to %s:%d could not not established." ip port
    in
    print_endline (msg ^ " Trying to (re)connect in 10s.");
    change_status false;
    let* () = Lwt_unix.sleep 10. in
    Lwt.catch connect handle_exception
  in
  Lwt.catch connect handle_exception

let register _ _ _ = Ok ""

(* Web server *)

open Opium.Std

let headers mime =
  Cohttp.Header.init_with "content-type" mime

let register_stream status event (uri, fstream) =
  get uri (fun _req ->
    let headers = headers "text/event-stream" in
    let* str = fstream status event in
    `String str |> respond' ~headers)

let register_static status history (uri, fpage) =
  get uri (fun _req -> 
    let headers = headers "text/json" in
    `String (fpage status (S.value history)) |> respond' ~headers)

let register_page (uri, fpage) =
  get uri (fun _req -> 
    let headers = headers "text/html" in
    `Html (fpage version) |> respond' ~headers)

let register_file (uri, path, mime) =
  get uri (fun _req ->
    let headers = headers mime in
    `String (Web.file_content path) |> respond' ~headers)

let register_context_update set_ctx uri =
  post uri (fun req ->
    let+ pairs = App.urlencoded_pairs_of_body req in
    Web.update_context set_ctx pairs;
    Response.create ~code:(`Code 204) ())

let serve_webpage port set_ctx status event =
  let history = S.fold (fun l e -> e :: l) [] event in
  let server () = App.empty
  |> App.port port
  |> List.fold_right (register_stream status event) Web.api_stream
  |> List.fold_right (register_static status history) Web.api_static
  |> List.fold_right (register_file) Web.files
  |> List.fold_right (register_page) Web.pages
  |> register_context_update set_ctx Web.api_context
  |> App.start in
  Lwt.async server;
  Printf.printf "Webserver listens at localhost:%d\n" port;
  fst (Lwt.wait ())

(* Main program *)

open Lwt_react

let main ?(ip=ip)
         ?(port=port)
         ?(webport=webport) 
         ?(_token="") () =

  (* Reactive signals and events *)
  let status, set_status = S.create (Status.make ~ip ~port false) in
  let event, push_event = E.create () in
  let ctx, set_ctx = E.create () in
  let webserver = serve_webpage webport set_ctx status event in
  let trainserver = connect_train_server set_status push_event ctx ip port in
  Debug.log_status_events status event;
  List.iter push_event (Debug.test_events 10);
  Lwt.pick [webserver; trainserver]

let () = Lwt_main.run @@ main ()

