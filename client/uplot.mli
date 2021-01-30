
type t
type cache = (string * float array) list
type render

val empty : ?width  : int ->
            ?height : int ->
            ?title  : string -> unit -> t

val xaxis : ?label : string ->
            ?format_as_time : bool ->
            ?data : float array -> t -> t

val series : ?label : string ->
             ?stroke : string ->
             ?width : float ->
             ?fill : string ->
             ?dash : string ->
             data : float array -> t -> t

val series_cache : ?stroke : (string -> string option) ->
                   ?width : (string -> float option) ->
                   ?fill : (string -> string option) -> 
                   ?dash : (string -> string option) ->
                   data : cache -> t -> t

val render : string -> t -> render
val compatible : cache -> render -> bool
val update : ?reset : bool ->
             cache -> render -> render option


val cache_length : cache -> int
