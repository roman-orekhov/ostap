open Matcher
open String
open Str
open Ostap 

let p = rule <name>=IDENT -"(" (<h>=IDENT <t>=(-"," IDENT)* {h::t}) -")" end
let ws      = regexp "[ \n\t\r]+"
let ident   = regexp "[a-zA-Z_]\([a-zA-Z_0-9]\)*"

class lexer s p coord =

    object (self)

       inherit [lexer] matcher (fun s p coord -> new lexer s p coord) s p coord

       method skip =
          
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
  match p (ofString s) with
  | Parsed (x, _) -> Printf.printf "Success: %s\n" (P.toString x)
  | Failed msgs | Error msgs -> Printf.printf "Unsuccess: %s\n" (let module M = View.List (Msg) in M.toString msgs)

let _ =
  parse "a (b, c, d)";
  parse "a"
  


