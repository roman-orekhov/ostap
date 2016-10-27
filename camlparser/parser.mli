open Ostap
open Combinators

type ('a, 'b) stream =
   < getCHAR : ('a, Matcher.Token.t, 'b) parsed;
     getEOF : ('a, Matcher.Token.t, 'b) parsed;
     getFLOAT : ('a, Matcher.Token.t, 'b) parsed;
     getINFIXOP0 : ('a, Matcher.Token.t, 'b) parsed;
     getINFIXOP1 : ('a, Matcher.Token.t, 'b) parsed;
     getINFIXOP2 : ('a, Matcher.Token.t, 'b) parsed;
     getINFIXOP3 : ('a, Matcher.Token.t, 'b) parsed;
     getINFIXOP4 : ('a, Matcher.Token.t, 'b) parsed;
     getINT : ('a, Matcher.Token.t, 'b) parsed;
     getINT32 : ('a, Matcher.Token.t, 'b) parsed;
     getINT64 : ('a, Matcher.Token.t, 'b) parsed;
     getLABEL : ('a, Matcher.Token.t, 'b) parsed;
     getLIDENT : ('a, Matcher.Token.t, 'b) parsed;
     getNATIVEINT : ('a, Matcher.Token.t, 'b) parsed;
     getOPTLABEL : ('a, Matcher.Token.t, 'b) parsed;
     getPREFIXOP : ('a, Matcher.Token.t, 'b) parsed;
     getSTRING : ('a, Matcher.Token.t, 'b) parsed;
     getUIDENT : ('a, Matcher.Token.t, 'b) parsed;
     look : string -> ('a, Matcher.Token.t, 'b) parsed;
     regexp :
      ?except:(string -> bool) ->
      string ->
      string ->
      string -> ('a, Matcher.Token.t, 'b) parsed
     .. > as 'a

val implementation :  (('a, 'b) stream as 'c, unit, 'b) cont -> 'c -> 'b steps
val interface :       (('a, 'b) stream as 'c, unit, 'b) cont -> 'c -> 'b steps
val toplevel_phrase : (('a, 'b) stream as 'c, unit, 'b) cont -> 'c -> 'b steps
val use_file :        (('a, 'b) stream as 'c, unit, 'b) cont -> 'c -> 'b steps