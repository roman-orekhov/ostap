open Ostap

class ['a] lexer :
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
    method getAMPERAMPER : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getAMPERSAND : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getAND : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getAS : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getASSERT : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getBACKQUOTE : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getBANG : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getBAR : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getBARBAR : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getBARRBRACKET : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getBEGIN : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getCHAR : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getCLASS : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getCOLON : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getCOLONCOLON : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getCOLONEQUAL : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getCOLONGREATER : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getCOMMA : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getCONSTRAINT : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getDO : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getDONE : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getDOT : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getDOTDOT : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getDOWNTO : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getELSE : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getEND : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getEOF : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getEQUAL : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getEXCEPTION : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getEXTERNAL : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getFALSE : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getFLOAT : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getFOR : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getFUN : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getFUNCTION : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getFUNCTOR : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getGREATER : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getGREATERRBRACE : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getGREATERRBRACKET : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getIF : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getIN : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getINCLUDE : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getINFIXOP0 : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getINFIXOP1 : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getINFIXOP2 : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getINFIXOP3 : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getINFIXOP4 : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getINHERIT : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getINITIALIZER : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getINT : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getINT32 : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getINT64 : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getLABEL : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getLAZY : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getLBRACE : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getLBRACELESS : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getLBRACKET : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getLBRACKETBAR : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getLBRACKETGREATER : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getLBRACKETLESS : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getLESS : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getLESSMINUS : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getLET : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getLIDENT : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getLPAREN : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getMATCH : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getMETHOD : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getMINUS : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getMINUSDOT : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getMINUSGREATER : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getMODULE : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getMUTABLE : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getNATIVEINT : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getNEW : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getOBJECT : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getOF : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getOPEN : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getOPTLABEL : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getOR : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getPLUS : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getPLUSDOT : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getPREFIXOP : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getPRIVATE : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getQUESTION : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getQUESTIONQUESTION : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getQUOTE : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getRBRACE : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getRBRACKET : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getREC : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getRPAREN : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getSEMI : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getSEMISEMI : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getSHARP : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getSIG : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getSTAR : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getSTRING : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getSTRUCT : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getTHEN : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getTILDE : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getTO : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getTRUE : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getTRY : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getTYPE : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getUIDENT : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getUNDERSCORE : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getVAL : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getVIRTUAL : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getWHEN : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getWHILE : ('b, Matcher.Token.t, 'a) Combinators.parsed
    method getWITH : ('b, Matcher.Token.t, 'a) Combinators.parsed
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
    method skip :
      int ->
      Msg.Coord.t ->
      [ `Failed of Msg.t | `Skipped of int * Msg.Coord.t ]
  end
