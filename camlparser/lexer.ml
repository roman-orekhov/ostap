open Ostap
open Matcher
open Printf

(* To convert integer literals, allowing max_int + 1 (PR#4210) *)

let cvt_int_literal s =
  - int_of_string ("-" ^ s)
let cvt_int32_literal s =
  Int32.neg (Int32.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_int64_literal s =
  Int64.neg (Int64.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_nativeint_literal s =
  Nativeint.neg (Nativeint.of_string ("-" ^ String.sub s 0 (String.length s - 1)))

module Keywords =
  struct

    module S = Set.Make (String)

    let k = 
      Array.fold_left 
        (fun s n -> S.add n s) 
        S.empty 
        [|
        "and";
        "as";
        "assert";
        "begin";
        "class";
        "constraint";
        "do";
        "done";
        "downto";
        "else";
        "end";
        "exception";
        "external";
        "false";
        "for";
        "fun";
        "function";
        "functor";
        "if";
        "in";
        "include";
        "inherit";
        "initializer";
        "lazy";
        "let";
        "match";
        "method";
        "module";
        "mutable";
        "new";
        "object";
        "of";
        "open";
        "or";
(*      "parser"; *)
        "private";
        "rec";
        "sig";
        "struct";
        "then";
        "to";
        "true";
        "try";
        "type";
        "val";
        "virtual";
        "when";
        "while";
        "with";
        "mod";
        "land";
        "lor";
        "lxor";
        "lsl";
        "lsr";
        "asr"
        |]

    let check name = S.mem name k

  end

class ['b] lexer s =
   let skip_line_directive =
      let e = Str.regexp "#[ \t]*[0-9]+[ \t]*\(\"[^\r\n\"]*\"\)?[^\r\n]*" in
      (fun s p ->
        try 
          if Str.string_match e s p 
          then `Skipped (p+(String.length (Str.matched_string s)))
          else `Skipped p  
        with Not_found -> `Skipped p
      ) in
   let skip  = Skip.create [Skip.whitespaces " \n\t\r\012"; Skip.nestedComment "(*" "*)"; skip_line_directive] in
   let lowercase = "[a-z\223-\246\248-\255_]" in
   let uppercase = "[A-Z\192-\214\216-\222]" in
   let identchar = "[A-Za-z_\192-\214\216-\246\248-\255'0-9]" in
   let symbolchar = "[-!$%&*+./:<=>?@^|~]" in
   let decimal_literal = "[0-9][0-9_]*" in
   let hex_literal = "0[xX][0-9A-Fa-f][0-9A-Fa-f_]*" in
   let oct_literal = "0[oO][0-7][0-7_]*" in
   let bin_literal = "0[bB][0-1][0-1_]*" in
   let int_literal = sprintf "%s\|%s\|%s\|%s" decimal_literal hex_literal oct_literal bin_literal in
   let float_literal = Str.regexp "[0-9][0-9_]*\(\.[0-9_]*\)?\([eE][-+]?[0-9][0-9_]*\)?" in
   let prefixop = Str.regexp (sprintf "[!~?]%s+" symbolchar) in
   let infixop0 = Str.regexp (sprintf "!=\|[=<>|&$]%s*" symbolchar) in
   let infixop1 = Str.regexp (sprintf "[@^]%s*" symbolchar) in
   let infixop2 = Str.regexp (sprintf "[-+]%s*" symbolchar) in
   let infixop3 = Str.regexp (sprintf "\(mod\|land\|lor\|lxor\)\\b\|[*/%%]%s*" symbolchar) in
   let infixop4 = Str.regexp (sprintf "\(lsl\|lsr\|asr\)\\b\|\*\*%s*" symbolchar) in
   let lident = Str.regexp (sprintf "%s%s*" lowercase identchar) in
   let uident = Str.regexp (sprintf "%s%s*" uppercase identchar) in
   let label = Str.regexp (sprintf "~%s%s*:" lowercase identchar) in
   let optlabel = Str.regexp (sprintf "?%s%s*:" lowercase identchar) in
   let check_label s =
      let name = String.sub s 1 (String.length s - 2) in
      Keywords.check name in
   let int32       = Str.regexp (sprintf "%sl" int_literal) in
   let int64       = Str.regexp (sprintf "%sL" int_literal) in
   let nativeint   = Str.regexp (sprintf "%sn" int_literal) in
   let int_literal = Str.regexp int_literal in
   let check_int32     s = try ignore (cvt_int32_literal     s); false with Failure _ -> true in
   let check_int64     s = try ignore (cvt_int64_literal     s); false with Failure _ -> true in
   let check_nativeint s = try ignore (cvt_nativeint_literal s); false with Failure _ -> true in
   let check_int       s = try ignore (cvt_int_literal       s); false with Failure _ -> true in
   let escaped = "\\\\\([\\'\"ntbr ]\|[0-9][0-9][0-9]\|x[0-9A-Fa-f][0-9A-Fa-f]\)" in
   let char_literal = Str.regexp (sprintf "'\([^\\']\|%s\)'" escaped) in
   let string = Str.regexp "\"\([^\"\]\|\\\\\(.\|\n\)\)*\"" in
   
   object (self)

      inherit ['b] Matcher.t s

      method skip = skip s

      method getPREFIXOP = self#get "prefixop"     prefixop      "!!"
      method getINFIXOP0 = self#get "infixop0"     infixop0      "!="
      method getINFIXOP1 = self#get "infixop1"     infixop1      "@"
      method getINFIXOP2 = self#get "infixop2"     infixop2      "+"
      method getINFIXOP3 = self#get "infixop3"     infixop3      "mod"
      method getINFIXOP4 = self#get "infixop4"     infixop4      "lsl"
      method getLIDENT   = self#get "lident"       lident        "a"   ?except:(Some Keywords.check)
      method getUIDENT   = self#get "uident"       uident        "A"
      method getLABEL    = self#get "label"        label         "~a:" ?except:(Some check_label)
      method getOPTLABEL = self#get "optlabel"     optlabel      "?a:" ?except:(Some check_label)
      method getFLOAT    = self#get "float_literal"float_literal "0.0"
      method getINT32    = self#get "int32"        int32         "0l"  ?except:(Some check_int32    )
      method getINT64    = self#get "int64"        int64         "0L"  ?except:(Some check_int64    )
      method getNATIVEINT= self#get "nativeint"    nativeint     "0n"  ?except:(Some check_nativeint)
      method getINT      = self#get "int_literal"  int_literal   "0"   ?except:(Some check_int      )
      method getCHAR     = self#get "char_literal" char_literal  "'a'"
      method getSTRING   = self#get "string"       string        "\"\""

      method getAND = self#get "keyword and" (Str.regexp "and\\b") "and "
      method getAS = self#get "keyword as" (Str.regexp "as\\b") "as "
      method getASSERT = self#get "keyword assert" (Str.regexp "assert\\b") "assert "
      method getBEGIN = self#get "keyword begin" (Str.regexp "begin\\b") "begin "
      method getCLASS = self#get "keyword class" (Str.regexp "class\\b") "class "
      method getCONSTRAINT = self#get "keyword constraint" (Str.regexp "constraint\\b") "constraint "
      method getDO = self#get "keyword do" (Str.regexp "do\\b") "do "
      method getDONE = self#get "keyword done" (Str.regexp "done\\b") "done "
      method getDOWNTO = self#get "keyword downto" (Str.regexp "downto\\b") "downto "
      method getELSE = self#get "keyword else" (Str.regexp "else\\b") "else "
      method getEND = self#get "keyword end" (Str.regexp "end\\b") "end "
      method getEXCEPTION = self#get "keyword exception" (Str.regexp "exception\\b") "exception "
      method getEXTERNAL = self#get "keyword external" (Str.regexp "external\\b") "external "
      method getFALSE = self#get "keyword false" (Str.regexp "false\\b") "false "
      method getFOR = self#get "keyword for" (Str.regexp "for\\b") "for "
      method getFUN = self#get "keyword fun" (Str.regexp "fun\\b") "fun "
      method getFUNCTION = self#get "keyword function" (Str.regexp "function\\b") "function "
      method getFUNCTOR = self#get "keyword functor" (Str.regexp "functor\\b") "functor "
      method getIF = self#get "keyword if" (Str.regexp "if\\b") "if "
      method getIN = self#get "keyword in" (Str.regexp "in\\b") "in "
      method getINCLUDE = self#get "keyword include" (Str.regexp "include\\b") "include "
      method getINHERIT = self#get "keyword inherit" (Str.regexp "inherit\\b") "inherit "
      method getINITIALIZER = self#get "keyword initializer" (Str.regexp "initializer\\b") "initializer "
      method getLAZY = self#get "keyword lazy" (Str.regexp "lazy\\b") "lazy "
      method getLET = self#get "keyword let" (Str.regexp "let\\b") "let "
      method getMATCH = self#get "keyword match" (Str.regexp "match\\b") "match "
      method getMETHOD = self#get "keyword method" (Str.regexp "method\\b") "method "
      method getMODULE = self#get "keyword module" (Str.regexp "module\\b") "module "
      method getMUTABLE = self#get "keyword mutable" (Str.regexp "mutable\\b") "mutable "
      method getNEW = self#get "keyword new" (Str.regexp "new\\b") "new "
      method getOBJECT = self#get "keyword object" (Str.regexp "object\\b") "object "
      method getOF = self#get "keyword of" (Str.regexp "of\\b") "of "
      method getOPEN = self#get "keyword open" (Str.regexp "open\\b") "open "
      method getOR = self#get "keyword or" (Str.regexp "or\\b") "or "
(*    method getPARSER = self#get "keyword parser" (Str.regexp "parser\\b") "parser " *)
      method getPRIVATE = self#get "keyword private" (Str.regexp "private\\b") "private "
      method getREC = self#get "keyword rec" (Str.regexp "rec\\b") "rec "
      method getSIG = self#get "keyword sig" (Str.regexp "sig\\b") "sig "
      method getSTRUCT = self#get "keyword struct" (Str.regexp "struct\\b") "struct "
      method getTHEN = self#get "keyword then" (Str.regexp "then\\b") "then "
      method getTO = self#get "keyword to" (Str.regexp "to\\b") "to "
      method getTRUE = self#get "keyword true" (Str.regexp "true\\b") "true "
      method getTRY = self#get "keyword try" (Str.regexp "try\\b") "try "
      method getTYPE = self#get "keyword type" (Str.regexp "type\\b") "type "
      method getVAL = self#get "keyword val" (Str.regexp "val\\b") "val "
      method getVIRTUAL = self#get "keyword virtual" (Str.regexp "virtual\\b") "virtual "
      method getWHEN = self#get "keyword when" (Str.regexp "when\\b") "when "
      method getWHILE = self#get "keyword while" (Str.regexp "while\\b") "while "
      method getWITH = self#get "keyword with" (Str.regexp "with\\b") "with "


      method getUNDERSCORE = self#get "underscore" (Str.regexp "_\\b") "_ "
      method getTILDE = self#look "~"
      method getQUESTION = self#look "?"
      method getQUESTIONQUESTION = self#look "??"
      method getSHARP = self#look "#"
      method getAMPERSAND = self#look "&"
      method getAMPERAMPER = self#look "&&"
      method getBACKQUOTE = self#look "`"
      method getQUOTE = self#look "'"
      method getLPAREN = self#look "("
      method getRPAREN = self#look ")"
      method getSTAR = self#look "*"
      method getCOMMA = self#look ","
      method getMINUSGREATER = self#look "->"
      method getDOT = self#look "."
      method getDOTDOT = self#look ".."
      method getCOLON = self#look ":"
      method getCOLONCOLON = self#look "::"
      method getCOLONEQUAL = self#look ":="
      method getCOLONGREATER = self#look ":>"
      method getSEMI = self#look ";"
      method getSEMISEMI = self#look ";;"
      method getLESS = self#look "<"
      method getLESSMINUS = self#look "<-"
      method getEQUAL = self#look "="
      method getLBRACKET = self#look "["
      method getLBRACKETBAR = self#look "[|"
      method getLBRACKETLESS = self#look "[<"
      method getLBRACKETGREATER = self#look "[>"
      method getRBRACKET = self#look "]"
      method getLBRACE = self#look "{"
      method getLBRACELESS = self#look "{<"
      method getBAR = self#look "|"
      method getBARBAR = self#look "||"
      method getBARRBRACKET = self#look "|]"
      method getGREATER = self#look ">"
      method getGREATERRBRACKET = self#look ">]"
      method getRBRACE = self#look "}"
      method getGREATERRBRACE = self#look ">}"
      method getBANG = self#look "!"

      method getPLUS = self#look "+"
      method getPLUSDOT = self#look "+."
      method getMINUS = self#look "-"
      method getMINUSDOT = self#look "-."

   end
