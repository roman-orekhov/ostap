(*
 * Test022: an example of lexical analyser written in Ostap.
 * Copyright (C) 2006
 * Dmitri Boulytchev, St.Petersburg State University
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file COPYING).
 *)

open Checked
open Ostap
open Printf

let pl = List.fold_left (^) ""

type stream = {data: string; pos: int}

let ofString s = {data=s; pos=0}

let eof s = if s.pos = String.length s.data then Ok ('$', fail, s) else Fail []

let between x y = 
  (fun s ->
    if s.pos < String.length s.data 
    then 
      let a = s.data.[s.pos] in
      if a >= x && a <= y 
      then Ok (a, fail, {s with pos=s.pos+1})
      else Fail []
    else Fail []
  )

let isA x = 
  (fun s ->
    if s.pos < String.length s.data 
    then 
      let a = s.data.[s.pos] in
      if a = x 
      then Ok (a, fail, {s with pos=s.pos+1})
      else Fail []
    else Fail []
  )

let whitespace = (isA ' ') <|> (isA '\n') <|> (isA '\t')
let letter     = (between 'a' 'z') <|> (between 'A' 'Z')
let digit      = between '0' '9'
let sign       = (isA '+') <|> (isA '-') <|> (isA '*') <|> (isA '/')
let punct      = (isA ',') <|> (isA '.') <|> (isA '|') <|> (isA '(') <|> (isA ')') <|> (isA ':') <|> (isA ';')

let ident   = map (fun (hd, tl) -> hd :: tl) (letter |> (<*>) (letter <|> digit))
let literal = (<+>) digit

let lexem = 
  (map (fun x -> [x]) whitespace) <|> ident <|> literal <|> (map (fun x -> [x]) sign) <|> (map (fun x -> [x]) punct)

let _ = 
  let parse = (<*>) lexem |> eof in
  let print = function
    | Ok ((list, _), _, s) -> 
	printf "Parsed: ";
	List.iter 
	  (fun lexem ->
	    List.iter print_char lexem;
	    printf "; "
	  ) 
	  list;
	printf ", rest=%s\n" (pl s)

    | Fail _ -> 
	printf "Failed\n"
  in
  match parse (ofString "int x; x+1;") with
  | Ok ((list, _), _, s) -> 
	printf "Parsed: ";
	List.iter 
	  (fun lexem ->
	    List.iter print_char lexem;
	    printf " "
	  ) 
	  list;
	printf "\n"

  | Fail _ -> printf "Failed\n"


