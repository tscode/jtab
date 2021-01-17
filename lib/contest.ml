
type t = {
    reqid : int
  ; client : string
  ; era : int
  ; names : string list
  ; matches : int
  ; balance : int list list
  ; elo : float list
  ; draw : float
  ; sadv : float
} [@@deriving yojson]

let dummy () =
  let sum l = List.fold_left (+) 0 l in
  let balance =
    let f _ = List.init 5 (fun _ -> Random.int 20 - 10) in List.init 5 f
  in
  { reqid = Random.int 50
  ; client = "best-contest-client"
  ; era = Random.int 20
  ; names = ["mcts-1500"; "mcts-750"; "mcts-250"; "mcts-10"; "model*"]
  ; matches = (List.map sum balance |> sum) + Random.int 100
  ; balance
  ; elo = List.init 5 (fun _ -> Random.float 1500.)
  ; draw = Random.float 200.
  ; sadv = Random.float 200. }

let list_sorti cmp l =
  let idx = List.init (List.length l) Fun.id in
  List.sort cmp (List.combine idx l) |> List.split |> snd

let sort c =
  let cmp (i,_) (j,_) =
    - Float.compare (List.nth c.elo i) (List.nth c.elo j) in
  { c with
    names = list_sorti cmp c.names
  ; balance = List.map (list_sorti cmp) c.balance |> list_sorti cmp
  ; elo = list_sorti cmp c.elo }


