

type source =
  [ `TCP of string * int
  | `File of string
  | `History of string * string
  | `Debug ]
  [@@deriving yojson]

type t =
  [ `Connecting of source
  | `Ok of source
  | `Error of source * string ]
  [@@deriving yojson]

let hash (s : t) = Hashtbl.hash s

let source_to_json source =
  source_to_yojson source |> Yojson.Safe.to_string

let source_of_json str =
  try Yojson.Safe.from_string str |> source_of_yojson with
  | _ -> Error "cannon parse source"

let to_json status =
  to_yojson status |> Yojson.Safe.to_string

let of_json str =
  try Yojson.Safe.from_string str |> of_yojson with
  | _ -> Error "cannot parse status"

let source_to_string = function
  | `File path -> "file://" ^ path
  | `TCP (host, port) -> Printf.sprintf "tcp://%s:%d" host port
  | `Debug -> "<debug mode>"
  | `History (name, _) -> "tmp://" ^ name

let parse_source ?(host="127.0.0.1") ?(port=7789) src =
  let get_port str =
    try Some (int_of_string str) with
    | _ -> None
  in
  let parse_tcp host port src =
    let l = String.length src in
    let tcp h p = `TCP (h, p) in
    match String.sub src 0 6 = "tcp://" with
    | false | exception _ -> None
    | true ->
      match String.sub src 6 (l-6) |> String.split_on_char ':' with
      | "" :: [] -> tcp host port |> Option.some
      | host :: [] -> tcp host port |> Option.some
      | "" :: port :: [] -> Option.map (tcp host) (get_port port)
      | host :: port :: [] -> Option.map (tcp host) (get_port port)
      | _ -> None
  in
  let parse_file src =
    let l = String.length src in
    match String.sub src 0 7 = "file://" with
    | false | exception _ -> None
    | true -> Some (`File (String.sub src 7 (l-7)))
  in
  match parse_tcp host port src with
  | Some _ as source -> source
  | None ->
  match parse_file src with
  | Some _ as source -> source
  | None ->
  (* automatic inference of protocol *)
  match String.split_on_char ':' src with
  | [_] -> parse_file ("file://" ^ src)
  | _ -> parse_tcp host port ("tcp://" ^ src)
