open Msg

module MC : Map.S with type key = Coord.t

type r = (int * (string * Coord.t)) list MC.t

val debug        : bool ref
val toLineDir    : Locator.t -> string -> string
val stripLines   : string -> r * string
val addFirst     : r -> r
val toString     : r -> string
val getSuccReloc : string -> r -> Coord.t -> r * (string * Coord.t)
(** works only before calling Locator.updateToString *)
val printReloc   : string -> r -> Coord.t * Coord.t -> unit

