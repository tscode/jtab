

type arch =
  | Primitive of string
  | Composite of string * arch list
  [@@deriving yojson]

type t = {
    name : string  
  ; info : string
  ; trunk : arch
  ; phead : arch
  ; vhead : arch
  ; fhead : arch option
  ; params : int
  ; base : string option
} [@@deriving yojson]


let get_name m = m.name
let get_info m = m.info
let get_base m = m.base
let get_params m = m.params

