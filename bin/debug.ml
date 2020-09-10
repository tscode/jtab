open Jtable

open Lwt_react

let hd_opt = function
  | [] -> None
  | h :: _ -> Some h

let log_status_events _status history =
  let event = E.fmap hd_opt (S.changes history) in
 (* let status = S.changes status in
  E.map (fun st -> print_endline (Msg.status_to_json st)) status |> E.keep;*)
  E.map (fun ev -> print_endline (Event.summary ev)) event |> E.keep

let test_events n = let open Event in
  let rand () = Random.float 1. in
  let loss () =
    { policy = rand ()
    ; value = rand ()
    ; feature = rand ()
    ; total = rand () }
  in
  let epoch number = Epoch
    { number
    ; trainloss = loss ()
    ; testloss = loss ()
    ; quality = rand ()
    ; size = Random.int 1000 + 50
    ; ctxid = 0
    ; era = 1 }
  in
  [ event ~id:1 (Context (Context.dummy 0))
  ; event ~id:2 (Datareq {reqid = 42; ctxid = 14; era = "testera"})
  ; event ~id:3 (Era {name = "testera"; number = 5; epoch = 1}) ]
  @
  List.init n (fun i -> event ~id:(i+4) (epoch i))
  @
  [ event ~id:(n+4) (Context (Context.dummy 1))
  ; event ~id:(n+5) (Datareq {reqid = 42; ctxid = 14; era = "testera"})
  ; event ~id:(n+6) (Era {name = "testera"; number = 5; epoch = 1})
  ; event ~id:(n+7) (Contest (Contest.dummy ()))
  ; event ~id:(n+8) (Contest (Contest.dummy () |> Contest.sort))
  ; event ~id:(n+9) (Contest (Contest.dummy ())) ]
  |> List.rev
