
print_endline "ocaml says hello in javascript"

open Jtable

open Lwt
open Lwt.Syntax
open Lwt_react

open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml

(* xml query *)

let get_xml uri =
  let+ response = XmlHttpRequest.get uri in
  match response.code with
  | 200 -> Ok response.content
  | code ->
    let msg = Printf.sprintf "Could not connect to %s:%d\n" uri code in
    Error msg


(* query the connection status *)

let rec poll_status uri set =
  let* response = get_xml uri in
  match Result.bind response Status.of_json with
  | Ok status -> set status; poll_status uri set
  | Error msg -> prerr_endline msg |> return

let rec init_status_polling uri_static uri_stream =
  let+ status =
    let+ response = get_xml uri_static in
    match Result.bind response Status.of_json with
    | Ok status -> status
    | Error msg -> prerr_endline msg; Status.make ~ip:"error" ~port:0 false
  in
  let status, set_status = S.create status in
  ignore_result (poll_status uri_stream set_status);
  status


(* query events *)

let rec poll_events uri add_event history =
  let* response = get_xml uri in
  match Result.bind response Event.parse_event_list with
  | Ok events -> List.iter add_event events;
    poll_events uri add_event history
  | Error msg -> prerr_endline msg |> return

let init_event_polling uri_static uri_stream =
  let event, add_event = E.create () in
  let+ prior_events =
    let+ response = get_xml uri_static in
    Result.map print_endline response |> ignore;
    match Result.bind response Event.parse_event_list with
    | Ok events -> events
    | Error msg -> prerr_endline msg; []
  in
  let history = S.fold (fun l e -> e :: l) prior_events event in
  ignore_result (poll_events uri_stream add_event history);
  event, history


(* script for the EVENTS tab *)

let event_searchstring () =
  let input =
    Dom_html.getElementById_exn "event-search"
    |> Dom_html.CoerceTo.input
    |> Js.Opt.to_option
    |> Option.get
  in
  let searchstring, push = S.create None in
  let get_value el =
    el##.value |> Js.to_string |> function "" -> None | v -> Some v
  in
  let handle _ = get_value input |> push; Js._true in
  input##.oninput := Dom_html.handler handle;
  searchstring

let event_selector () =
  let ids =
    [ "epochs", `Epochs
    ; "eras", `Eras
    ; "data", `Data
    ; "contests", `Contests
    ; "context", `Context
    ; "clients", `Clients ]
  in
  let connect_button (id, id') =
    let signal, set = S.create (Some id') in
    let button = Dom_html.getElementById_exn ("event-list-select-" ^ id) in
    let handle _ =
      set (match (S.value signal) with None -> Some id' | Some _ -> None);
      button##.classList##toggle (Js.string "active")
    in
    button##.onclick := Dom_html.handler handle;
    button##.classList##add (Js.string "active");
    signal
  in
  S.merge (fun l s -> s :: l) [] (List.map connect_button ids)
  |> S.map (List.filter_map Fun.id)

let match_selector ev selector =
  match (selector, ev.Event.body) with
  | `Epochs, Epoch _ | `Eras, Era _ | `Data, Data _
  | `Data, Datareq _ | `Context, Context _
  | `Contests, Contest _ | `Contests, Contestreq _
  | `Clients, Client _ -> true
  | _ -> false

(* TODO: improve this matching function *)
let match_search ev = function
  | None -> true
  | Some str -> 
    let re = Regexp.regexp_case_fold str in 
    Regexp.string_match re (Event.to_json ev) 0
    |> Option.is_some

let is_visible (searchstring, selection) ev =
  List.exists (match_selector ev) selection && match_search ev searchstring

let summary_entry_tyxml ev =
  let open Event in
  let open Tyxml_js.Html in
  let time =
    let a = [a_class ["event-time"]] in
    span ~a [Event.time_to_string ev.time |> txt] in
  let number =
    let a = [a_class ["event-number"]] in
    span ~a [Printf.sprintf "%d" ev.id |> txt] in
  let text =
    let a = [a_class ["event-summary"]] in
    span ~a [Event.summary ev |> txt] in
  let entry =
    let id = Printf.sprintf "event:%d" ev.Event.id |> a_id in
    let cl = ["event-list-entry"] |> a_class in
    li ~a:[id; cl] [number; time; text]
  in entry

let prependChild el child =
  Dom.insertBefore el child (el##.firstChild)

let insert_summary_entry summary_list active set_active filter ev =
  let entry = summary_entry_tyxml ev |> Tyxml_js.To_dom.of_li in
  let handle _ =
    let () = match S.value active with
    | None -> ()
    | Some index ->
      let id = Printf.sprintf "event:%d" index in
      let active_entry = Dom_html.getElementById_exn id in
      active_entry##.classList##remove (Js.string "active");
    in
    entry##.classList##add (Js.string "active");
    set_active (Some ev.id);
    Js._true
  in
  entry##.onclick := Dom_html.handler handle;
  if not (is_visible (S.value filter) ev)
  then entry##.classList##add (Js.string "hidden");
  prependChild summary_list entry

let connect_filter summary_list history filter =
  let display crit =
    let hist = S.value history in
    let dom_hist = summary_list##.childNodes |> Dom.list_of_nodeList in
    let f ev li =
      let li = Dom.CoerceTo.element li
      |> Js.Opt.to_option
      |> Option.get
      |> Dom_html.element
      in
      match is_visible crit ev with
      | true -> li##.classList##remove (Js.string "hidden")
      | false -> li##.classList##add (Js.string "hidden")
    in
    List.iter2 f hist dom_hist
  in
  E.keep (E.map display (S.changes filter))

let show_details history active =
  let pre = Dom_html.getElementById_exn "event-details" in
  let replace_time ev json =
    let f = function
    | "time", _ -> "time", `String (Event.datetime_to_string ev.Event.time)
    | a -> a
    in`Assoc (Yojson.Safe.Util.to_assoc json |> List.map f)
  in
  let f = function
    | None -> ()
    | Some index ->
      let hist = S.value history in
      let n = List.length hist - index in
      let ev = List.nth (S.value history) n in
      let str = Event.to_yojson ev
      |> replace_time ev
      |> Yojson.Safe.pretty_to_string
      |> fun x -> Regexp.global_replace (Regexp.regexp "\"") x "" in
      pre##.innerHTML := Js.string str;
  in
  E.map f (S.changes active) |> E.keep

let init_events_tab event history =
  let search = event_searchstring () in
  let selector = event_selector () in
  let filter = S.Pair.pair search selector in
  let active, set_active = S.create None in
  let summary_list = Dom_html.getElementById_exn "event-list" in
  let insert ev =
    insert_summary_entry summary_list active set_active filter ev
  in
  List.iter insert (S.value history |> List.rev);
  E.keep (E.map insert event);
  connect_filter summary_list history filter;
  show_details history active

let init_tabs () =
  let keys = ["events"; "context"; "contests"; "model"; "clients"] in
  let tabs = List.map ((^) "tab-") keys |> List.map Dom_html.getElementById in
  let btts = List.map ((^) "menu-tab-") keys |> List.map Dom_html.getElementById in
  let hide t = t##.classList##add (Js.string "hidden") in
  let unact b = b##.classList##remove (Js.string "active") in
  let connect_button tab button =
    let handle () =
      List.iter hide tabs;
      List.iter unact btts;
      tab##.classList##remove (Js.string "hidden");
      button##.classList##add (Js.string "active")
    in
    button##.onclick := Dom_html.handler (fun _ -> handle (); Js._true);
    if tab = List.hd tabs then handle ()
  in
  List.iter2 connect_button tabs btts

let init_connection_status status =
  let el = Dom_html.getElementById_exn "menu-connection-status" in
  let set_status_msg s = match s.Status.connected with
  | false ->
    let msg = Printf.sprintf "not connected (%s:%d)" s.ip s.port in
    el##.textContent := (Js.string msg |> Js.Opt.return);
    el##.classList##remove (Js.string "connected")
  | true ->
    let msg = Printf.sprintf "connected (%s:%d)" s.ip s.port in
    el##.textContent := (Js.string msg |> Js.Opt.return);
    el##.classList##add (Js.string "connected")
  in
  set_status_msg (S.value status);
  E.map set_status_msg (S.changes status) |> E.keep

(* run the javascript for the menu and all tabs *)

let () =
  let promise =
    let* event, history = init_event_polling "/api/events" "/api/event-stream" in
    let* status = init_status_polling "/api/status" "/api/status-stream" in
    let () = init_connection_status status in
    let () = init_tabs () in
    let () = init_events_tab event history in
    fst (Lwt.wait ())
  in
  ignore_result promise

