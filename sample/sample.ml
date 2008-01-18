open Matcher
open String
open Str
open Ostap 

let p = rule <name>=IDENT -"(" (<h>=IDENT <t>=(-"," IDENT)* {h::t}) -")" end

let q = rule "{" IDENT+ "}" {()} | IDENT {()} end
let t = rule "match" q? "in" IDENT end

let ws      = regexp "[ \n\t\r]+"
let ident   = regexp "[a-zA-Z_]\([a-zA-Z_0-9]\)*"

class lexer s p coord =

    object (self)

       inherit matcher s p coord

       method skip p coord =
          
	      if string_match ws s p

	      then

	         let m = matched_string s in

	         (p+length m), (shiftPos coord m 0 (length m))

	      else p, coord

       method getIDENT = self#get "identifier" ident

    end

let ofString s = new lexer s 0 (1, 1)

let parse s =
  let module P = View.NamedPair (struct let first = "name" let second = "args" end) (Token) (View.List (Token)) in
  match t (ofString s) with
  | Parsed (x, _) -> Printf.printf "Success\n" (*P.toString x*)
  | Failed msgs | Error msgs -> Printf.printf "Unsuccess: %s\n" (let module M = View.List (Msg) in M.toString msgs)

let _ =
  parse "match {b c, d} in x"
  


