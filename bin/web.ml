open Jtable

let css =
  [ "/files/uPlot.min.css"
  ; "/files/jtab.css" ]

let js =
  [ "/files/uPlot.iife.min.js"
  ; "/files/jtab.js" ]


open Tyxml
open Lwt_react


(* Stream API *)

let event_stream _ event =
  let f ev = Event.events_to_string [ev] in
  E.map f event |> E.next

let status_stream status _ =
  S.changes status |> E.map Status.to_json |> E.next

let api_stream =
  [ "/api/event-stream", event_stream
  ; "/api/status-stream", status_stream ]


(* Static API *)

let events_static _ history =
  Event.events_to_string history

let status_static status _ =
  S.value status |> Status.to_json

let api_static =
  [ "/api/events", events_static
  ; "/api/status", status_static ]


(* Set context via post *)
let update_context set_ctx pairs =
  let id = ("id", `Int (-1)) in
  let f (key, vals) = (key, Yojson.Safe.from_string (List.hd vals)) in
  let ctx = `Assoc (id :: List.map f pairs) |> Context.of_yojson in
  match ctx with
  | Ok c -> set_ctx c
  | Error err -> prerr_endline err 

let api_context = "/api/submit-context"

(* Serve files *)

let file_content path =
  let rec read_all acc ic = match input_line ic with
  | exception End_of_file -> close_in ic; String.concat "\n" (List.rev acc)
  | line -> read_all (line :: acc) ic
  in
  match open_in path with
  | exception _ -> Printf.sprintf "alert('cannot open file %s')" path
  | ic -> read_all [] ic

let files =
  [ "/files/jtab.js", "js/jtab.js", "text/javascript"
  ; "/files/jtab.css", "css/jtab.css", "text/css"
  ; "/files/uPlot.iife.min.js", "js/uPlot.iife.min.js", "text/javascript"
  ; "/files/uPlot.min.css", "css/uPlot.min.css", "text/css"
  ]


(* Main page *)

(* Events tab *)
let events_tab = let open Html in

  (* first column *)
  let event_list =
    let selectors = 
      let id = a_id "events-table-selectors" in
      let cl = a_class ["event-selector"; "active"] in
      div ~a:[id]
      [ button ~a:[cl; a_id "events-table-select-epochs"]   [txt "epochs"]
      ; button ~a:[cl; a_id "events-table-select-eras"]     [txt "eras"]
      ; button ~a:[cl; a_id "events-table-select-data"]     [txt "data"]
      ; button ~a:[cl; a_id "events-table-select-contests"] [txt "contests"]
      ; button ~a:[cl; a_id "events-table-select-context"]  [txt "context"]
      ; button ~a:[cl; a_id "events-table-select-clients"]  [txt "clients"] ]
    in
    let search =
      let id = "event-search" in
      input ~a:[a_id id; a_input_type `Text; a_placeholder "search"] ()
    in
    let filter =
      let title = div ~a:[a_class ["title"]] [txt "filter"] in
      let container = div ~a:[a_class ["container"]] [selectors; search] in
      div ~a:[a_id "event-filter"] [title; container]
    in
    let list =
      let title = div ~a:[a_class ["title"]] [txt "history"] in
      let container =
        let tab =
          let id = a_id "events-event-table" in
          let cl = a_class ["event-table"] in
          table ~a:[id; cl] [] in
        let cl = a_class ["container"] in
        div ~a:[cl] [tab]
      in
      div [title; container]
    in
    let id = a_id "events-table-column" in
    let cl = a_class ["column"] in
    div ~a:[id; cl] [filter; list]

  in

  (* second column *)
  let event_details =
    let title = div ~a:[a_class ["title"]] [txt "details"] in
    let preview = pre ~a:[a_id "event-details"] [] in
    let box = div ~a:[a_class ["container"]] [preview] in
    let id = a_id "event-details-column" in
    let cl = a_class ["column"] in
    div ~a:[id; cl] [title; box]

  in

  (* third column *)
  let event_progress =
    let title = div ~a:[a_class ["title"]] [txt "progress"] in
    let sel = select ~a:[a_id "event-progress-select"]
      [ option ~a:[a_value "loss"] (txt "loss")
      ; option ~a:[a_value "quality"] (txt "quality")
      ; option ~a:[a_value "elo"] (txt "elo") ]
    in
    let box = 
      let graph = div ~a:[a_id "event-progress-graph"] [] in
      let cl = a_class ["container"] in
      div ~a:[a_id "event-progress"; cl] [sel; graph]
    in
    let id = a_id "event-progress-column" in
    let cl = a_class ["column"] in
    div ~a:[id; cl] [title; box]

  in

  let id = a_id "tab-events" in
  let cl = a_class ["tab"] in
  div ~a:[id; cl] [ event_list ; event_details ; event_progress ]


(* Context tab *)

let context_tab = let open Html in
  
  (* first column *)
  let context_list =
    let title = div ~a:[a_class ["title"]] [txt "contexts"] in
    let container =
      let tab =
        let id = a_id "context-event-table" in
        let cl = a_class ["event-table"] in
        table ~a:[id; cl] [] in
      div ~a:[a_class ["container"]] [ tab ]
    in
    let id = a_id "context-table-column" in
    let cl = a_class ["column"] in
    div ~a:[id; cl] [title; container]

  in

  (* second column *)
  let context_form =
    let title = div ~a:[a_class ["title"]] [txt "update context"] in
    let container = div ~a:[a_id "context-form-container"] [] in
    let id = a_id "context-form-column" in
    let cl = a_class ["column"] in
    div ~a:[id; cl] [title; container]

  in
  
  let id = a_id "tab-context" in
  let cl = a_class ["tab"; "hidden"] in
  div ~a:[id; cl] [ context_list; context_form ]


(* Contests tab *)

let contests_tab = let open Html in

  (* first column *)
  let contests_list =
    let title = div ~a:[a_class ["title"]] [txt "contests"] in
    let container =
      let tab =
        let id = a_id "contests-event-table" in
        let cl = a_class ["event-table"] in
        table ~a:[id; cl] [] in
      div ~a:[a_class ["container"]] [ tab ]
    in
    let id = a_id "contests-list-column" in
    let cl = a_class ["column"] in
    div ~a:[id; cl] [title; container]

  in

  (* second column *)
  let contests_results =
    let title = div ~a:[a_class ["title"]] [txt "results"] in
    let container =
      let cl = a_class ["container"] in
      let id = a_id "contests-results-container" in
      div ~a:[id; cl] []
    in
    let id = a_id "contests-results-column" in
    let cl = a_class ["column"] in
    div ~a:[id; cl] [title; container]

  in

  (* third column *)
  let contests_matches =
    let title = div ~a:[a_class ["title"]] [txt "matches"] in
    let container =
      let tab =
        let id = a_id "contests-matches-table" in
        table ~a:[id] [] in
      let cl = a_class ["container"] in
      let id = a_id "contests-matches-container" in
      div ~a:[id; cl] [tab]
    in
    let id = a_id "contests-matches-column" in
    let cl = a_class ["column"] in
    div ~a:[id; cl] [title; container]

  in

  let id = a_id "tab-contests" in
  let cl = a_class ["tab"; "hidden"] in
  div ~a:[id; cl] [ contests_list; contests_results; contests_matches ]


let model_tab = let open Html in
  let id = a_id "tab-model" in
  let cl = a_class ["tab"; "hidden"] in
  div ~a:[id; cl] [ txt "TODO" ]

let clients_tab = let open Html in
  let id = a_id "tab-clients" in
  let cl = a_class ["tab"; "hidden"] in
  div ~a:[id; cl] [ txt "TODO" ]

let webpage version = let open Html in
  let head =
    let title = title (txt "Jtab - jtac table") in
    let enc = meta ~a:[a_charset "utf-8"] () in
    let link_css =
      List.map (fun href -> link ~rel:[`Stylesheet] ~href ()) css
    in
    let link_js =
      List.map (fun src -> script ~a:[a_src src] (txt "")) js
    in
    head title (enc :: link_css @ link_js)
  in
  let spanclass = a_class ["nowrap"; "vcenter"] in
  let logo =
    let ver = span ~a:[a_id "version"] [txt version] in
    let title = span ~a:[a_id "logo"] [txt "jtab"] in
    span ~a:[spanclass] [span [title; ver]]
  in
  let tabs =
    let cl = a_class ["menu-tab"] in
    span ~a:[spanclass]
    [ button ~a:[cl; a_id "menu-tab-events"] [txt "events"]
    ; button ~a:[cl; a_id "menu-tab-context"] [txt "context"]
    ; button ~a:[cl; a_id "menu-tab-contests"] [txt "contests"]
    ; button ~a:[cl; a_id "menu-tab-model"] [txt "model"]
    ; button ~a:[cl; a_id "menu-tab-clients"] [txt "clients"] ]
  in
  let status = span ~a:[a_id "menu-connection-status"; spanclass] [] in
  let menu = div ~a:[a_id "menu-bar"] (logo :: tabs :: [status]) in
  let body = body
    [menu; events_tab; context_tab; contests_tab; model_tab; clients_tab]
  in
  Format.asprintf "%a" (pp ()) (html head body)

let pages =
  [ "/", webpage
  ; "/overview", webpage ]


