

let read path =
  let rec read_all acc ic = match input_line ic with
  | exception End_of_file -> close_in ic; String.concat "\n" (List.rev acc)
  | line -> read_all (line :: acc) ic
  in
  match open_in path with
  | exception _ -> Printf.sprintf "alert('cannot open file %s')" path
  | ic -> read_all [] ic

let data =
  [ "/static/jtab.js"          , "js/jtab.js"          , "text/javascript"
  ; "/static/jtab.css"         , "css/jtab.css"        , "text/css"
  ; "/static/uPlot.iife.min.js", "js/uPlot.iife.min.js", "text/javascript"
  ; "/static/uPlot.min.css"    , "css/uPlot.min.css"   , "text/css"
  ]


