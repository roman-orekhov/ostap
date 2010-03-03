type 'a t =
    Test of string * ('a -> bool)
  | Aster of 'a t
  | Plus of 'a t
  | Opt of 'a t
  | Alter of 'a t list
  | Juxt of 'a t list
  | Arg of string * 'a t
  | BOS
  | EOS

val toText : 'a t -> Pretty.printer

val toString : 'a t -> string

module Diagram :
  sig

    type 'a expr = 'a t
    type 'a t 

    val toDOT : 'a t -> string
    val make  : 'a expr -> [ `Duplicate of string | `Ok of 'a t ]

  end
