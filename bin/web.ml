open Jtable

let css = "

  body {
    margin: 0px;
    height: 100%;
  }

  h3 {

  }

  ul.no-bullets {
    list-style-type: none;
    padding: 0;
    margin: 0;
  }

  .nowrap {
    white-space: nowrap;
  }

  .vcenter {
    display: inline-flex;
    align-items: center
  }

  .title {
    text-align: center;
    font-family: sans-serif;
    color: #666;
    padding-top: 1ex;
    padding-bottom: 0.5ex;
  }

  .menu-tab.active {
    background-color: #ddecf9;
  }

  details {
      padding: .2em .2em 0;
  }

  summary {
      margin: -.2em -.2em 0;
      padding: .2em;
  }

  details[open] {
      padding: .2em;
  }

  details[open] summary {
      color: #aa6633
      margin-bottom: .2em;
  }

  div.tab {
    padding: 1ex 0px;
  }

  div#menu-bar {
    padding: 0px 3%;
    box-shadow: 0px 0.5px 5px #333;
  }

  #logo {
    font-weight: bolder;
    font-size: x-large;
  }

  #version {
    padding-left: 0.5em;
    font-weight: normal;
    font-size: large;
    color: #666;
  }

  ul#event-list {
    width: 100%
    min-height: 200px;
    max-height: 100%;
  }

  li.event-list-entry {
    cursor: pointer;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
    font-family: monospace;
    padding: 0px .75em;
    border-radius: 4px;
  }

  .event-list-entry:hover {
    background-color: #eef7ff;
  }

  .event-list-entry.active {
    background-color: #ddecf9;
  }

  .hidden {
    display: none !important;
  }
  
  ul#event-list {
    overflow: auto;
  }

  input#event-search {
    width: 100%;
    margin: 1.5ex 0px;
    box-sizing: border-box;
  }

  button {
    font-weight: bold;
    padding-top: 1ex;
    padding-bottom: 1ex;

    border: 0px solid black;
    background-color: white;

    font-size: 16px;
    cursor: pointer;
  }

  button:hover {
    background-color: #eef7ff;
  }


  button.event-selector {

    font-weight: bold;
    padding-top: 1ex;
    padding-bottom: 1ex;

    border: 0px solid black;
    background-color: white;
    color: #999;

    font-size: 16px;
    cursor: pointer;
  }

  button.event-selector.active:hover {
    background-color: white;
  }


  button.event-selector:hover {
    background-color: white;
  }

  button.event-selector.active {
    color: green
  }

  div#tab-events {
    display: grid;
    grid-column-gap: 20px;
    grid-template-columns: 25em 25em auto;
    margin: 0px 3%;
    height: 100%;
  }

  #menu-bar {
    display:flex;
    justify-content: space-between;
  }

  button.menu-tab {
    padding: 1ex 1em;
  }

  #menu-connection-status {
    font-family: sans-serif;
    color: #b34e4e;
    font-weight: bold;
    font-size: small;
  }

  #menu-connection-status.connected {
    color: green;
  }

  div#event-list-selectors {
    display: grid;
    grid-column-gap: 0px;
    grid-template-columns: 33% 33% 33%;
    border: 1px solid #cbcbcb;
    border-radius: 4px;
  }

  div#event-list-container {
    width: 100%;
    padding: 1ex .5em;
    border: 1px solid #cbcbcb;
    border-radius: 4px;
    box-sizing: border-box;
  }

  .container-title {
    margin-bottom: 1ex;
    text-align: center;
  }

  .event-time {
    color: #060;
    padding-right: 1em;
    font-weight: bold;
  }

  .event-number {
    color: #555;
    font-weight: bold;
    padding-right: 0.5em;
  }

  .event-summary {
    color: #333333;
  }

  div#event-details-box {
    padding: 1ex .5em;
    border: 1px solid #cbcbcb;
    border-radius: 4px;
    box-sizing: border-box;
    font-size: 8pt;
  }

  pre#event-details {
    margin: 0px 0px;
    overflow-x: auto;
    min-height: 10ex;
  }

"

let js = ""

open Tyxml
open Lwt_react


(* Stream API *)

let event_stream _ event =
  let f ev = Event.event_list_to_json [ev] in
  E.map f event |> E.next

let status_stream status _ =
  S.changes status |> E.map Status.to_json |> E.next

let api_stream =
  [ "/api/event-stream", event_stream
  ; "/api/status-stream", status_stream ]


(* Static API *)

let events_static _ history =
  Event.event_list_to_yojson history |> Yojson.Safe.to_string

let status_static status _ =
  S.value status |> Status.to_json

let api_static =
  [ "/api/events", events_static
  ; "/api/status", status_static ]


(* Serve javascript *)

let js_content path =
  let rec read_all acc ic = match input_line ic with
  | exception End_of_file -> String.concat "\n" (List.rev acc)
  | line -> read_all (line :: acc) ic
  in
  match open_in path with
  | exception _ -> "alert('jtable js file not found')"
  | ic -> read_all [] ic

let js_uri = "/api/js"


(* Pages *)

(* Events tab *)
let events_tab = let open Html in

  (* first column *)
  let event_list =
    let selectors = 
      let id = a_id "event-list-selectors" in
      let cl = a_class ["event-selector"; "active"] in
      div ~a:[id]
      [ button ~a:[cl; a_id "event-list-select-epochs"]   [txt "epochs"]
      ; button ~a:[cl; a_id "event-list-select-eras"]     [txt "eras"]
      ; button ~a:[cl; a_id "event-list-select-data"]     [txt "data"]
      ; button ~a:[cl; a_id "event-list-select-contests"] [txt "contests"]
      ; button ~a:[cl; a_id "event-list-select-context"]  [txt "context"]
      ; button ~a:[cl; a_id "event-list-select-clients"]  [txt "clients"] ]
    in
    let search =
      let id = "event-search" in
      input ~a:[a_id id; a_input_type `Text; a_placeholder "search"] ()
    in
    let filter =
      let title = div ~a:[a_class ["title"]] [txt "filter"] in
      div ~a:[a_id "event-filter"] [title; selectors; search]
    in
    let list =
      let title = div ~a:[a_class ["title"]] [txt "stream"] in
      let container = div ~a:[a_id "event-list-container"]
        [ ul ~a:[a_id "event-list"; a_class ["no-bullets"]] [] ] in
      div [title; container]
    in
    div ~a:[a_id "event-list-column"] [filter; list]

  in

  (* second column *)
  let event_details =
    let title = div ~a:[a_class ["title"]] [txt "details"] in
    let preview = pre ~a:[a_id "event-details"] [] in
    let box = div ~a:[a_id "event-details-box"] [preview] in
    div ~a:[a_id "event-details-column"] [title; box]

  in

  (* third column *)
  let event_progress =
    let box = div ~a:[a_id "event-progress"] [] in
    div ~a:[a_id "event-progress-container"] [box]

  in

  let id = a_id "tab-events" in
  let cl = a_class ["tab"] in
  div ~a:[id; cl] [ event_list ; event_details ; event_progress ]


(* Context tab *)

let json_to_tyxml ?(label_class=[]) ?(input_class=[])
                  ?(div_class=[]) ?(prefix="") json =
  let open Html in
  let create_div (key, value) =
    let k = prefix ^ key in
    let lab = label ~a:[a_label_for k; a_class label_class] [txt key] in
    let inp =
      let typ = a_input_type `Text in
      let default = a_value (Yojson.Safe.to_string value) in
      input ~a:[a_id k; a_name k; a_class input_class; typ; default] ()
    in
    div ~a:[a_class div_class] [lab; inp]
  in
  let open Yojson.Safe in
  match from_string json |> Util.to_assoc with
  | pairs -> List.map create_div pairs |> Result.ok
  | exception _ -> Result.error "Invalid json"

let pairs_to_yojson pairs =
  let f (k,v) = (k, Yojson.Safe.from_string v) in
  match List.map f pairs with
  | lst -> `Assoc lst |> Result.ok
  | exception _ -> Result.error "Invalid json"


let context_tab = let open Html in
  
  (* first column *)
  let context_list =
    let title = div ~a:[a_class ["title"]] [txt "recorded contexts"] in
    let container = div ~a:[a_id "context-list-container"]
      [ ul ~a:[a_id "context-list"; a_class ["no-bullets"]] [] ] in
    div [title; container]

  in

  (* second column *)
  let context_form =
    let title = div ~a:[a_class ["title"]] [txt "update context"] in
    let form = form ~a:[a_id "context-form"] [] in
    div ~a:[a_id "context-form-container"] [title; form]

  in
  
  let id = a_id "tab-context" in
  let cl = a_class ["tab"; "hidden"] in
  div ~a:[id; cl] [ context_list; context_form ]

let contests_tab = let open Html in
  let id = a_id "tab-contests" in
  let cl = a_class ["tab"; "hidden"] in
  div ~a:[id; cl] [ txt "TODO" ]

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
    let title = title (txt "Jtac training table - overview") in
    let enc = meta ~a:[a_charset "utf-8"] () in
    let css = style [txt css] in
    let js = script (txt js) in
    let jsoc = script ~a:[a_src "/api/js"] (txt "") in
    head title [enc; css; js; jsoc]
  in
  let spanclass = a_class ["nowrap"; "vcenter"] in
  let logo =
    let ver = span ~a:[a_id "version"] [txt version] in
    let title = span ~a:[a_id "logo"] [txt "jtable"] in
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


