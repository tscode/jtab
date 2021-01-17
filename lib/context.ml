
type t = {
    id : int
  ; name : string
  ; power : int
  ; temperature : float
  ; exploration : float
  ; dilution : float
  ; init_steps : int * int
  ; branch : float
  ; branch_steps : int * int
  ; augment : bool
  ; min_playings : int
  ; max_playings : int
  ; epoch_size : int
  ; iterations : int
  ; test_frac : float
  ; max_age : int
  ; max_use : int
  ; min_quality : float
  ; age_weight : float
  ; capacity : int
  ; era_size : int
  ; batch_size : int
  ; learning_rate : float
  ; momentum : float
  ; loss_weights : float list
  ; backups : int
  ; msg : string (* additional message, like reason for context change *)
} [@@deriving yojson]

let dummy id = {
    id
  ; name = "dummy-context"
  ; power = 50
  ; temperature = 1.
  ; exploration = 1.41
  ; dilution = 0.
  ; init_steps = (0, 0)
  ; branch = 0.
  ; branch_steps = (0, 0)
  ; augment = true
  ; min_playings = 10
  ; max_playings = 250
  ; epoch_size = 30000
  ; iterations = 1
  ; test_frac = 0.05
  ; max_age = 3
  ; max_use = 3
  ; min_quality = 0.3
  ; age_weight = 0.5
  ; capacity = 1000000
  ; era_size = 100000
  ; batch_size = 1024
  ; learning_rate = 1e-3
  ; loss_weights = [1.; 1.; 1.]
  ; momentum = 0.9
  ; backups =  3
  ; msg = "dummy context for testing purposes"
}

let parse str =
  Yojson.Safe.from_string str |> of_yojson

let to_json ctx =
  to_yojson ctx |> Yojson.Safe.to_string
