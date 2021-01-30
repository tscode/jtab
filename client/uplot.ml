
open Js_of_ocaml

type options =
  { label : string option
  ; stroke : string option
  ; width : float option
  ; fill : string option
  ; dash : string option }

type series =
  { opts : options
  ; data : float array }

type x_axis =
  { data : float array
  ; label : string option
  ; format_as_time : bool }

type t =
  { x_axis : x_axis
  ; series : series list
  ; title : string
  ; width : int
  ; height : int }

type cache =
  (string * float array) list

type render =
  { labels : string list
  ; js_obj : Js.Unsafe.any }

let series ?label ?stroke ?width ?fill ?dash ~data p =
  let s = {data; opts = {label; stroke; width; fill; dash}} in
  { p with series = s :: p.series}

let series_cache ?stroke ?width ?fill ?dash ~data p =
  let f gr (name, values) =
    let apply g = g name in
    let stroke = Option.bind stroke apply in
    let width = Option.bind width apply in
    let fill = Option.bind fill apply in
    let dash = Option.bind dash apply in
    series ?width ?stroke ?fill ?dash ~data:values ~label:name gr
  in
  List.fold_left f p data

let xaxis ?label ?(format_as_time=false) ?(data=[||]) p =
  let x = {data; label; format_as_time} in { p with x_axis = x }

let width w p = { p with width = w }
let height h p = { p with height = h }
let title t p = { p with title = t }

let empty ?(width=300) ?(height=250) ?(title="") () =
  let x_axis = { data = [||]; label = None; format_as_time = false } in
  { x_axis; series = []; title; width; height }

let auto_x_axis t l =
  { data = Array.init l Float.of_int
  ; label = t.label
  ; format_as_time = t.format_as_time }

let series_length (s : series) = Array.length s.data

let inject = Js.Unsafe.inject
let inject_array v = Js.array v |> inject
let inject_data v = Array.of_list v |> inject_array
let inject_string s = Js.string s |> inject

let inject_series_data v =
  let f x = match Float.is_nan x with
  | true -> Js.null
  | false -> Js.Opt.return x in
  Array.map f v |> inject_array

let inject_opt v = Js.Opt.option v |> inject
let inject_string_opt v = Option.map Js.string v |> Js.Opt.option |> inject

let series_data (s : series) = inject_series_data s.data
let x_data x = inject_series_data x.data
let x_options t = [| "label", inject_string_opt t.label |] |> Js.Unsafe.obj

let series_options s =
  [| "label", inject_string_opt s.opts.label
  ;  "stroke", inject_string_opt s.opts.stroke
  ;  "width", inject_opt s.opts.width
  ;  "fill", inject_string_opt s.opts.fill
  ;  "dash", inject_string_opt s.opts.dash |]
  |> Js.Unsafe.obj

let render target p =
  let container = Dom_html.getElementById target |> inject in
  let x = match p.x_axis.data with
  | [||] -> auto_x_axis p.x_axis (List.hd p.series |> series_length)
  | _ -> p.x_axis
  in
  let data = x_data x :: List.map series_data p.series |> inject_data in
  let options =
    let series = x_options x :: List.map series_options p.series |> inject_data in
    let scales = match x.format_as_time with
    | true -> Js.Unsafe.obj [| |]
    | false -> Js.Unsafe.(obj [| "x", obj [|"time", Js._false |> inject |] |])
    in
    Js.Unsafe.obj [|
      "title", p.title |> inject_string
    ; "width", p.width |> inject
    ; "height", p.height |> inject
    ; "series", series
    ; "scales", scales
    |]
  in
  let args = [| options; data; container |] in
  let labels =
    let f x = x.opts.label |> Option.value ~default:"" in
    List.map f p.series in
  let js_obj = Js.Unsafe.(new_obj global##._uPlot args) in
  { labels ; js_obj }

(* check if a data cache is compatible with a rendered graph or
 * if the graph has to be redrawn *)
let compatible cache r =
  let rnames = r.labels |> List.sort String.compare in
  let cnames = cache |> List.map fst |> List.sort String.compare in
  rnames = List.filter ((<>) "x") cnames

(* update the data in a rendered graph *)
let update ?(reset=true) cache r =
  match compatible cache r with
  | false -> None
  | true ->
  let set data =
    let js_data = List.map inject_series_data data |> inject_data in
    let reset = Js.bool reset |> inject in
    Js.Unsafe.(meth_call r.js_obj "setData" [| js_data; reset |]) |> ignore;
    r
  in
  let data = match List.assoc_opt "x" cache with
  | Some x -> x :: List.map (fun l -> List.assoc l cache) r.labels
  | None ->
    let n = List.hd cache |> snd |> Array.length in
    let x = Array.init n Float.of_int in
    x :: List.map (fun l -> List.assoc l cache) r.labels
  in
  Some (set data)

let cache_length cache = match cache with
  | (_, h) :: _ -> Array.length h
  | [] -> 0

