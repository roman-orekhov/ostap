open Ostap

class ['a] lexer :
  string ->
  string ->
  object ('b)
    method col : int
    method coord : Msg.Coord.t
    method errors : Matcher.Errors.t list
    method get :
      ?except:(string -> bool) ->
      string ->
      Str.regexp ->
      string -> ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getCHAR : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getEOF : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getFLOAT : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getINFIXOP0 : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getINFIXOP1 : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getINFIXOP2 : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getINFIXOP3 : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getINFIXOP4 : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getINT : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getINT32 : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getINT64 : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getLABEL : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getLIDENT : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getNATIVEINT : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getOPTLABEL : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getPREFIXOP : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getSTRING : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getUIDENT : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method line : int
    method loc : Msg.Locator.t
    method look :
      string -> ('b, Matcher.Token.t, 'a) Combinators.parsed
    method pFuncCost :
      (int -> string option) * string * string ->
      [ `Exact of int | `Length ] ->
      ('b, Matcher.Token.t, 'a) Combinators.parsed
    method pos : int
    method prefix : int -> string
    method regexp :
      ?except:(string -> bool) ->
      string ->
      string ->
      string -> ('b, Matcher.Token.t, 'a) Combinators.parsed
    method reloc : Msg.Coord.t -> Msg.Locator.t
    method skip :
      int ->
      Msg.Coord.t ->
      [ `Failed of string | `Skipped of int * Msg.Coord.t ]
  end
