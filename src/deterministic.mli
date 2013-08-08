module type StreamChar =
  sig
    type t
    val compare : t -> t -> int
    val values : t list
    val toInt : t -> int
    val ofInt : int -> t
    val toString : t -> string
    val max : int
  end

module ASCIIStreamChar :
  sig
    type t = char
    val compare : 'a -> 'a -> int
    val values : char list
    val toInt : char -> int
    val ofInt : int -> char
    val toString : char -> string
    val max : int
  end

module DetNFA :
  functor (C : StreamChar) ->
    sig
      type state = {
        eos : int option;
        lookaheads : (t * int) list;
        args : (int * Regexp.Diagram.SS.t) Map.Make(Compare.String).t;
        symbols : (int * Regexp.Diagram.SS.t) array;
      }
      and t = {
        states : state array;
        ok : int list;
      }
      val make : C.t Regexp.Diagram.t -> t
      val printTable : t -> unit
    end
