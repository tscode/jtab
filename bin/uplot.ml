
open Js_of_ocaml

type options =
  { label : string option
  ; stroke : string option
  ; width : float option
  ; fill : string option
  ; dash : string option }

type series =
  { opts : options
  ; data : float list }

type time =
  { data : float list
  ; label : string option
  ; format_as_time : bool }

type t =
  { time : time
  ; series : series list
  ; title : string
  ; width : int
  ; height : int }

let series ?label ?stroke ?width ?fill ?dash ~data p =
  let s = {data; opts = {label; stroke; width; fill; dash}} in
  { p with series = s :: p.series}

let time ?label ?(format_as_time=false) ?(data=[]) p =
  let t = {data; label; format_as_time} in
  { p with time = t }

let width w p = { p with width = w }
let height h p = { p with height = h }
let title t p = { p with title = t }

let empty ?(width=300) ?(height=250) ?(title="") () =
  let time = { data = []; label = None; format_as_time = false } in
  { time; series = []; title; width; height }


let derive_time_data t l =
  { data = List.init l (fun i -> Float.of_int (i + 1))
  ; label = t.label
  ; format_as_time = t.format_as_time }

let series_length (s : series) = List.length s.data

let inject = Js.Unsafe.inject
let inject_data v = Array.of_list v |> Js.array |> inject
let inject_array v = Js.array v |> inject
let inject_string s = Js.string s |> inject

let inject_opt v = Js.Opt.option v |> inject
let inject_string_opt v = Option.map Js.string v |> Js.Opt.option |> inject


let time_data t = inject_data t.data
let series_data (s : series) = inject_data s.data

let time_options t =
  [| "label", inject_string_opt t.label |]
  |> Js.Unsafe.obj

let series_options s =
  [| "label", inject_string_opt s.opts.label
  ;  "stroke", inject_string_opt s.opts.stroke
  ;  "width", inject_opt s.opts.width
  ;  "fill", inject_string_opt s.opts.fill
  ;  "dash", inject_string_opt s.opts.dash |]
  |> Js.Unsafe.obj

let render target p =
  let time = match p.time.data with
  | [] -> derive_time_data p.time (List.hd p.series |> series_length)
  | _ -> p.time
  in
  let container =
    Dom_html.getElementById target
    |> inject
  in
  let data =
    time_data time :: List.map series_data p.series
    |> inject_data
  in
  let options =
    let series =
      time_options time :: List.map series_options p.series
      |> inject_data
    in
    let scales = match time.format_as_time with
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
  ignore Js.Unsafe.(new_obj global##._uPlot args)
  



