(*
 * Test001: simplest ocamlyard test.
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

open Str
open Ostap

class lexer s p = 
  object

    val ws    = regexp "[' ''\n''\t']+"
    val ident = regexp "[a-zA-Z]\([a-zA-Z0-9]\)*"

    method getIDENT =
      let p =
	if string_match ws s p 
	then p+(String.length (matched_string s))
	else p
      in
      if string_match ident s p
      then 
	let m = matched_string s in
	Parsed ((m, new lexer s (p+(String.length m))), [])
      else
	Failed [()]	

    method look x =
      let p =
	if string_match ws s p 
	then p+(String.length (matched_string s))
	else p
      in
      if string_match (regexp (quote x)) s p
      then 
	let m = matched_string s in
	Parsed ((m, new lexer s (p+(String.length m))), [])
      else
	Failed [()]	      

    method getEOF =
      let p =
	if string_match ws s p 
	then p+(String.length (matched_string s))
	else p
      in
      if p = String.length s 
      then
	Parsed (("<EOF>", new lexer s p), [])
      else
	Failed [()]	      
      
  end

let id = rule IDENT -EOF end 
let _ =
  begin match id (new lexer "   hasToBeParsed " 0) with
  | Parsed ((str, _), _) -> Printf.printf "Parsed: %s\n" str
  | _ -> Printf.printf "Failed.\n"
  end;
  begin match id (new lexer "   123 " 0) with
  | Parsed ((str, _), _) -> Printf.printf "Parsed: %s\n" str
  | _ -> Printf.printf "Failed.\n"
  end;
  