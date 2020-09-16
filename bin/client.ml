
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

let rec poll_status uri status set =
  let hash = Msg.hash_status (S.value status) in
  let uri' = Printf.sprintf "%s/%d" uri hash in
  let* response = get_xml uri' in
  match Result.bind response Msg.status_of_json with
  | Ok s -> set s; poll_status uri status set
  | Error msg -> prerr_endline msg |> return

let rec init_status_polling uri_static uri_stream =
  let+ status =
    let+ response = get_xml uri_static in
    match Result.bind response Msg.status_of_json with
    | Ok status -> status
    | Error msg -> prerr_endline msg; `Error (`Unknown, msg)
  in
  let status, set_status = S.create status in
  ignore_result (poll_status uri_stream status set_status);
  status

(* query events *)

let rec poll_events uri set_history history =
  let id = Event.latest_id (S.value history) in
  let uri' = Printf.sprintf "%s/%d" uri id in
  let* response = get_xml uri' in
  match Result.bind response Event.parse_events with
  | Error msg -> prerr_endline msg |> return
  | Ok events ->
    set_history (events @ S.value history);
    poll_events uri set_history history

let init_event_polling uri_static uri_stream =
  let+ prior_events =
    let+ response = get_xml uri_static in
    match Result.bind response Event.parse_events with
    | Ok events -> events
    | Error msg -> prerr_endline msg; []
  in
  let history, set = S.create prior_events in
  let event = E.map List.hd (S.changes history) in
  ignore_result (poll_events uri_stream set history);
  event, history


(* auxiliary function for displaying tables of events with
 * columns id | time | content *)

let remove_children el =
  let children = el##.childNodes |> Dom.list_of_nodeList in
  List.iter (Dom.removeChild el) children

let prepend_child el child =
  Dom.insertBefore el child (el##.firstChild)

let insert_event_row table active set id_conv text ev =
  let open Event in
  let row = let open Tyxml_js.Html in
    let index =
      let cl = a_class ["event-table-index"] in
      td ~a:[cl] [ev.id |> Int.to_string |> txt] in
    let time =
      let cl = a_class ["event-table-time"] in
      td ~a:[cl] [Event.time_to_string ev.time |> txt] in
    let content =
      let cl = a_class ["event-table-content"] in
      td ~a:[cl] [text |> txt] in
    let id = a_id (id_conv ev.id) in
    let cl = a_class ["event-table-row"] in
    tr ~a:[id; cl] [index; time; content]
    |> Tyxml_js.To_dom.of_tr
  in
  let handle _ =
    let () = match S.value active with
    | None -> ()
    | Some index ->
      let active_row = Dom_html.getElementById_exn (id_conv index) in
      active_row##.classList##remove (Js.string "active");
    in
    row##.classList##add (Js.string "active");
    (* pick up changes even when active element is selected *)
    set None; set (Some ev.id); Js._true
  in
  row##.onclick := Dom_html.handler handle;
  handle () |> ignore;
  prepend_child table row;
  row


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
    let button = Dom_html.getElementById_exn ("events-table-select-" ^ id) in
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

let connect_filter table history filter =
  let display crit =
    let hist = S.value history in
    let dom_hist = table##.childNodes |> Dom.list_of_nodeList in
    let f ev row =
      let tr = Dom.CoerceTo.element row
      |> Js.Opt.to_option
      |> Option.get
      |> Dom_html.element
      in
      match is_visible crit ev with
      | true -> tr##.classList##remove (Js.string "hidden")
      | false -> tr##.classList##add (Js.string "hidden")
    in
    List.iter2 f hist dom_hist
  in
  E.map display (S.changes filter) |> E.keep

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
      match List.find_opt (Event.has_id index) (S.value history) with
      | None -> prerr_endline (Printf.sprintf "event %d not found" index)
      | Some ev ->
        let str = Event.to_yojson ev
        |> replace_time ev
        |> Yojson.Safe.pretty_to_string
        |> fun x -> Regexp.global_replace (Regexp.regexp "\"") x "" in
        pre##.innerHTML := Js.string str
  in
  f (S.value active);
  E.map f (S.changes active) |> E.keep
  
let render_quality_graph id history =
  let open Event in
  let epoch_quality ev = match ev.body with
  | Epoch ep -> Some ep.quality
  | _ -> None
  in
  let data = List.filter_map epoch_quality history in
  Uplot.empty ~width:550 ~height:250 ()
  |> Uplot.time ~label:"epoch"
  |> Uplot.series ~label:"quality" ~data
  |> Uplot.render id

let rec find_index x = function
  | h :: t -> if h = x then 0 else 1 + find_index x t
  | [] -> -1

let palette n i =
  let s, l = 50, 50 in
  let h = (360. /. float_of_int n) *. (float_of_int i) in
  Printf.sprintf "hsl(%.0f, %d%%, %d%%)" h s l

let collect_names get_names collection =
  let f names x = List.sort_uniq String.compare (names @ get_names x) in
  List.fold_left f [] collection

let add_series color width data names graph =
  let f gr name =
    let stroke, width, data = color name, width name, data name in
    Uplot.series ~width ~stroke ~data ~label:name gr
  in
  List.fold_left f graph names

let render_loss_graph id history =
  let open Event in
  let epoch_loss ev = match ev.body with
  | Epoch ep -> Some (ep.trainloss, ep.testloss)
  | _ -> None in
  let train, test = List.filter_map epoch_loss history |> List.split in
  let render title ls =
    let names = collect_names (List.map fst) ls in
    let pal = palette (List.length names) in
    let color name = pal (find_index name names) in
    let width name = if name = "total" then 2. else 1. in
    let get_value name l =
      List.assoc_opt name l
      |> Option.value ~default:Float.nan
    in
    let data name = List.map (get_value name) ls in
    Uplot.empty ~width:550 ~height:250 ~title ()
    |> Uplot.time ~label:"epoch"
    |> add_series color width data names
    |> Uplot.render id
  in
  render "train" train;
  render "test" test

let render_elo_graph id history =
  let open Event in
  let open Contest in
  let get_contest ev = match ev.body with Contest ct -> Some ct | _ -> None in
  let contests = List.filter_map get_contest history in
  let players = collect_names (fun c -> c.names) contests in
  let pal = palette (List.length players) in
  let color player = pal (find_index player players) in
  let width player = if String.contains player '*' then 2. else 1. in
  let get_elo player contest =
    let assoc = List.combine contest.names contest.elo in
    List.assoc_opt player assoc
    |> Option.map Float.round
    |> Option.value ~default:Float.nan
  in
  let data player = List.map (get_elo player) contests in
  Uplot.empty ~width:550 ~height:400 ()
  |> Uplot.time ~label:"contest"
  |> add_series color width data players
  |> Uplot.render id

let show_progress_graph history =
  let id = "event-progress-graph" in
  let el = Dom_html.getElementById_exn "event-progress-select" in
  match Dom_html.CoerceTo.select el |> Js.Opt.to_option with
  | None -> prerr_endline "could not initialize progress graph"
  | Some el ->
    let selected, set = S.create (el##.value |> Js.to_string) in
    let handle _ = set (el##.value |> Js.to_string); Js._true in
    el##.onchange := Dom.handler handle;
    let f _ =
      remove_children (Dom_html.getElementById_exn id);
      match S.value selected with
      | "loss" -> render_loss_graph id (S.value history)
      | "quality" -> render_quality_graph id (S.value history)
      | "elo" -> render_elo_graph id (S.value history)
      | _ -> prerr_endline "unsupported graph selected"
    in
    (* TODO: E.l2 does not seem to work here? Why? *)
    f ();
    E.map f (S.changes selected) |> E.keep;
    E.map f (S.changes history) |> E.keep

let init_events_tab event history =
  let search = event_searchstring () in
  let selector = event_selector () in
  let filter = S.Pair.pair search selector in
  let active, set = S.create None in
  let table = Dom_html.getElementById_exn "events-event-table" in
  let insert ev =
    let id n = "event:" ^ Int.to_string n in
    let content = Event.summary ev in
    let row = insert_event_row table active set id content ev in
    if not (is_visible (S.value filter) ev)
    then row##.classList##add (Js.string "hidden")
  in
  List.iter insert (S.value history |> List.rev);
  E.keep (E.map insert event);
  connect_filter table history filter;
  show_details history active;
  show_progress_graph history

(* script for the CONTESTS tab *)

let get_contest index ev = let open Event in match ev.body with
  | Contest c -> if index = ev.id then Some c else None
  | _ -> None

let ranking_row rank (name, elo) = let open Tyxml_js.Html in
  tr ~a:[a_class ["contests-results-row"]]
  [ td ~a:[a_class ["event-table-index"]] [Int.to_string (rank+1) ^ "." |> txt]
  ; td ~a:[a_class ["event-table-time"]] [Printf.sprintf "%.0f" elo |> txt]
  ; td ~a:[a_class ["event-table-content"]] [name |> txt] ]

let ranking_table (c : Contest.t) =
  let rows = List.mapi ranking_row (List.combine c.names c.elo) in
  Tyxml_js.Html.(table ~a:[a_id "contests-results-table"] rows)

let meta_table (c : Contest.t) = let open Tyxml_js.Html in
  let cl_key = a_class ["event-table-index"] in
  let cl_val = a_class ["event-table-time"] in
  let matches =
    let key = td ~a:[cl_key] ["matches" |> txt] in
    let value = td ~a:[cl_val] [c.matches |> Int.to_string |> txt] in
    tr [key; value] in
  let sadv =
    let key = td ~a:[cl_key] ["start adv." |> txt] in
    let value = td ~a:[cl_val] [c.sadv |> Printf.sprintf "%.0f" |> txt] in
    tr [key; value] in
  let draw =
    let key = td ~a:[cl_key] ["draw range" |> txt] in
    let value = td ~a:[cl_val] [c.draw |> Printf.sprintf "%.0f" |> txt] in
    tr [key; value]
  in
  table ~a:[a_id "contest-results-meta-table"] [ matches; sadv; draw]

let show_results active history =
  let div = Dom_html.getElementById_exn "contests-results-container" in
  let update = function
  | None -> ()
  | Some index ->
    let cs = List.filter_map (get_contest index) (S.value history) in
    let c = List.hd cs |> Contest.sort in
    let ranking_tab = ranking_table c |> Tyxml_js.To_dom.of_table in
    let meta_tab = meta_table c |> Tyxml_js.To_dom.of_table in
    remove_children div;
    Dom.appendChild div ranking_tab;
    Dom.appendChild div meta_tab
  in
  update (S.value active);
  E.map update (S.changes active) |> E.keep

let matches_table (c : Contest.t) = let open Tyxml_js.Html in
  let len = List.length c.names in
  let index_class cl = a_class ("contests-matches-index" :: cl) in
  let index_row =
    let indices =
      let cl = index_class ["contests-first-row"] in
      let f i = td ~a:[cl] [Int.to_string (i+1) |> txt] in
      List.init len f in
    let cl = index_class ["contests-first-row"; "contests-first-col"] in
    tr (td ~a:[cl] [txt ""] :: indices)
  in
  let create_row i vals = 
    let cl = index_class ["contests-first-col"] in
    let sign = function
    | v when v < 0 -> a_class ["contests-matches-entry"; "negative"]
    | v when v > 0 -> a_class ["contests-matches-entry"; "positive"]
    | _ -> a_class ["contests-matches-entry"; "neutral"]
    in
    let f j v =
      if i = j then td [] 
      else td ~a:[sign v] [Int.abs v |> Int.to_string |> txt] in
    let data = List.mapi f vals in
    tr (td ~a:[cl] [Int.to_string (i+1) |> txt] :: data)
  in
  let rows = index_row :: List.mapi create_row c.balance in
  table ~a:[a_id "contests-matches-table"] rows


let show_matches active history =
  let div = Dom_html.getElementById_exn "contests-matches-container" in
  let update = function
  | None -> ()
  | Some index ->
    let cs = List.filter_map (get_contest index) (S.value history) in
    let c = List.hd cs |> Contest.sort in
    let tab = matches_table c |> Tyxml_js.To_dom.of_table in
    remove_children div;
    Dom.appendChild div tab
  in
  update (S.value active);
  E.map update (S.changes active) |> E.keep


let init_contests_tab event history =
  let id_str n = "contest:" ^ Int.to_string n in
  let active, set = S.create None in
  let table = Dom_html.getElementById_exn "contests-event-table" in
  let insert ev = let open Event in match ev.body with
  | Contest c ->
    let content = "contest of era " ^ Int.to_string c.era in
    insert_event_row table active set id_str content ev |> ignore
  | _ -> ()
  in
  List.iter insert (S.value history |> List.rev);
  E.map insert event |> E.keep;
  show_results active history;
  show_matches active history


(* script for the CONTEXT tab *)

let json_to_tyxml_entries ?(label_class=[]) ?(input_class=[])
                          ?(div_class=[]) ?(prefix="") json =
  let open Tyxml_js.Html in
  let create_div (key, value) =
    let k = prefix ^ key in
    let lab = label ~a:[a_label_for k; a_class label_class] [txt key] in
    let inp =
      let typ = a_input_type `Text in
      let default = a_placeholder (Yojson.Safe.to_string value) in
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
  
let xml_http_request ?(meth="GET") ?(header=[]) ?cb body uri =
  let req = XmlHttpRequest.create () in
  req##_open (Js.string meth) (Js.string uri) Js._true;
  List.iter
    (fun (k, v) -> req##setRequestHeader (Js.string k) (Js.string v))
    header;
  req##send (Js.string body |> Js.Opt.return);
  match cb with
  | None -> ()
  | Some cb ->
    let handle _ =
      let txt = Js.Opt.to_option req##.responseText in
      cb req##.status (Option.map Js.to_string txt); Js._true in
    req##.onload := Dom.handler handle

let submit_context_callback el =
  let btn =
    el##querySelector (Js.string "#context-submit-button")
    |> Js.Opt.to_option |> Option.get
  in
  let inputs = el##querySelectorAll (Js.string ".context-input") in
  let handle _ =
    let get_val x = match Js.to_string x##.value with
    | "" -> x##getAttribute (Js.string "placeholder")
      |> Js.Opt.to_option
      |> Option.value ~default:(Js.string "undefined")
      |> Js.to_string
    | value -> value
    in
    let yojson = inputs
    |> Dom.list_of_nodeList
    |> List.map Dom_html.CoerceTo.input
    |> List.filter_map Js.Opt.to_option
    |> List.map (fun x -> (Js.to_string x##.name, get_val x))
    |> List.cons ("id", "-1")
    |> pairs_to_yojson
    in
    match Result.bind yojson Context.of_yojson with
    | Error err -> prerr_endline err; Js._false
    | Ok ctx ->
      let json = Msg.context ctx |> Msg.to_json in
      let header = ["content-type", "text/json"] in
      let cb code txt = if code >= 400 then
        let msg = Option.value ~default:"no message" txt in
        Dom_html.window##alert (Js.string msg)
      in
      xml_http_request ~meth:"POST" ~header ~cb json "/api/msg";
      Js._true
  in
  btn##.onclick := Dom.handler handle
    
let show_context_form history active =
  let rec partition_list n acc = function
    | [] -> List.rev acc, []
    | h :: tl -> match List.length acc < n with
      | true -> partition_list n (h :: acc) tl
      | false -> List.rev acc, h :: tl
  in
  let get_context id ev = let open Event in match ev.body with
  | Context ctx -> if ev.id = id then Some ctx else None
  | _ -> None
  in
  let container = Dom_html.getElementById_exn "context-form-container" in
  let f = function
  | None -> ()
  | Some id ->
    let ctx = List.filter_map (get_context id) (S.value history) |> List.hd in
    let label_class = ["context-label"] in
    let input_class = ["context-input"] in
    let div_class = ["context-entry"] in
    let entries = Context.to_json ctx
      |> json_to_tyxml_entries ~label_class ~input_class ~div_class
      |> Result.map List.tl
    in
    match entries with
    | Error err -> prerr_endline ("Failed to parse context json: " ^ err)
    | Ok entries -> let open Tyxml_js.Html in
      let len = (List.length entries + 1) / 2 in
      let l1, l2 = partition_list len [] entries in
      let inner =
        let id = a_id "context-form-inner-container" in
        let cl = a_class ["container"] in
        div ~a:[id; cl] [ div l1; div l2]
      in
      let submit =
        let id = a_id "context-submit-button" in
        let cl = a_class ["button"] in
        div ~a:[a_class ["justify-right"]]
          [button ~a:[id; cl] [txt "submit"]]
      in
      let form =
        let id = a_id "context-form" in
       (* let action = a_action "/api/submit-context" in
        let meth = a_method `Post in *)
        div ~a:[id] [inner; submit]
      in
      let el = Tyxml_js.To_dom.of_div form in
      submit_context_callback el;
      match Dom.list_of_nodeList container##.childNodes with
      | [] -> Dom.appendChild container el
      | c :: _ -> Dom.replaceChild container el c
  in
  f (S.value active);
  E.map f (S.changes active) |> E.keep

let init_context_tab event history =
  let active, set = S.create None in
  let table = Dom_html.getElementById_exn "context-event-table" in
  let insert ev = let open Event in match ev.body with
  | Context ctx ->
    let id n = "context:" ^ Int.to_string n in
    let content = "context " ^ Int.to_string ctx.id in
    insert_event_row table active set id content ev |> ignore
  | _ -> ()
  in
  List.iter insert (S.value history |> List.rev);
  E.map insert event |> E.keep;
  show_context_form history active


(* script for the menu bar *)

let hide_element t = t##.classList##add (Js.string "hidden")
let show_element t = t##.classList##remove (Js.string "hidden")
let enact_element b = b##.classList##add (Js.string "active")
let unact_element b = b##.classList##remove (Js.string "active")

let init_tabs history =
  let keys = ["events"; "context"; "contests"; "model"; "clients"] in
  let tabs = List.map ((^) "tab-") keys |> List.map Dom_html.getElementById in
  let noev = Dom_html.getElementById "tab-no-events" in
  let btts = List.map ((^) "menu-tab-") keys |> List.map Dom_html.getElementById in
  let connect_button tab button =
    let handle () =
      List.iter hide_element tabs;
      hide_element noev;
      List.iter unact_element btts;
      enact_element button;
      match S.value history with
      | [] -> show_element noev
      | _ -> show_element tab
    in
    button##.onclick := Dom_html.handler (fun _ -> handle (); Js._true);
    if tab = List.hd tabs then handle ()
  in
  List.iter2 connect_button tabs btts;
  (* React on changes of the history. If it becomes empty, hide all tabs
   * and show the no-event message *)
  let no_events history =
    match history with
    | [] -> List.iter hide_element tabs; show_element noev
    | _ -> hide_element noev
  in
  E.map no_events (S.changes history) |> E.keep


let init_connection_status status =
  let format src str = let open Printf in
    let msg = match src with
    | `TCP (ip, port) -> sprintf "tcp://%s:%d%s" ip port (fst str)
    | `File fname -> sprintf "file://%s%s" fname (snd str)
    | `Unknown -> "unknown status" in
    Js.string msg |> Js.Opt.return
  in
  let el = Dom_html.getElementById_exn "menu-connection-status" in
  let set_status_msg s = match s with
  | `Connecting src ->
    el##.textContent := format src (" (connecting...)", " (opening...)");
    el##.classList##remove (Js.string "connected")
  | `Error (src, _err) ->
    el##.textContent := format src (" (not connected)", " (invalid)");
    el##.classList##remove (Js.string "connected")
  | `Ok src ->
    el##.textContent := format src ("", ""); 
    el##.classList##add (Js.string "connected")
  in
  set_status_msg (S.value status);
  E.map set_status_msg (S.changes status) |> E.keep


(* run javascript for the menu and all tabs *)

let () =
  let promise =

    let* () = Lwt_js_events.domContentLoaded () in
    let* event, history = init_event_polling "/api/events" "/api/event-stream" in
    let* status = init_status_polling "/api/status" "/api/status-stream" in
    let () = init_connection_status status in
    let () = init_tabs history in
    let () = init_events_tab event history in
    let () = init_context_tab event history in
    let () = init_contests_tab event history in
    fst (Lwt.wait ())
  in
  ignore_result promise

