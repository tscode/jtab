open Jtable

open Lwt_react

let log_status_events status event =
  let status = S.changes status in
  E.map (fun st -> print_endline (Status.to_json st)) status |> E.keep;
  E.map (fun ev -> print_endline (Event.summary ev)) event |> E.keep


let test_events = let open Event in
  let trainloss = { policy = 10.5; value = 0.8; feature = 0.2; total = 1.8 } in
  let testloss = { policy = 7.5; value = 1.5; feature = 0.3; total = 2.8 } in
  [
    event ~id:1 (Context Context.dummy)
  ; event ~id:2 (Datareq {reqid = 42; ctxid = 14; era = "testera"})
  ; event ~id:3 (Era {name = "testera"; number = 5; epoch = 1})
  ; event ~id:4 (Epoch {number = 1; trainloss; testloss; quality = 0.73; size = 88; ctxid = 0; era = 1})
]
