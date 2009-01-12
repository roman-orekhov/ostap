open Matcher
open String
open Str
open Ostap 
open Util

let p = ostap (name:IDENT -"(" (h:IDENT t:(-"," IDENT)* {h::t}) -")")

let q = ostap ("{" IDENT+ "}" {()} | IDENT {()})
let t = ostap ("match" q? "in" IDENT)

let parse s =
  let module P = View.NamedPair (struct let first = "name" let second = "args" end) (Token) (View.List (Token)) in
  match t (new lexer s) with
  | Parsed (x, _) -> Printf.printf "Success\n" 
  | Failed msgs -> 
      Printf.printf "Unsuccess: %s\n" 
	(match msgs with
	| None   -> "no description"
	| Some r -> r#toString `All `Desc
	)

let _ =
  parse "match {b c, d} in x"
  


