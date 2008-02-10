open Matcher
open String
open Str
open Ostap 

let p = rule <name>=IDENT -"(" (<h>=IDENT <t>=(-"," IDENT)* {h::t}) -")" end

let q = rule "{" IDENT+ "}" {()} | IDENT {()} end
let t = rule "match" q? "in" IDENT end

class lexer s  =
  let skip  = Skip.create [Skip.whitespaces " \n\t\r"] in
  let ident = regexp "[a-zA-Z_]\([a-zA-Z_0-9]\)*" in  
  object (self)
      
    inherit matcher s
	
    method skip p coord = skip s p coord
    method getIDENT = self#get "identifier" ident
	
  end

let parse s =
  let module P = View.NamedPair (struct let first = "name" let second = "args" end) (Token) (View.List (Token)) in
  match t (new lexer s) with
  | Parsed (x, _) -> Printf.printf "Success\n" 
  | Failed msgs | Error msgs -> Printf.printf "Unsuccess: %s\n" (let module M = View.List (Msg) in M.toString msgs)

let _ =
  parse "match {b c, d} in x"
  


