(*
 * Matcher: simple lexer pattern.
 * Copyright (C) 2006-2008
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

open Ostap
open String
open Printf
open Str

module Token =
  struct

    type t = string * Msg.Coord.t

    let toString (t, c) = sprintf "%s at %s" t (Msg.Coord.toString c)

    let loc (t, c) = Msg.Locator.Interval (c, ((fst c), (snd c)+(length t)-1))
    let repr       = fst

  end

let shiftPos (line, col) s b n =
  let rec inner i (line, col) =
    if i = n 
    then (line, col)
    else
      match s.[i] with
      | '\n' -> inner (i+1) (line+1, 1)
      | _    -> inner (i+1) (line, col+1)
  in
  inner b (line, col)

module Comment =
  struct

    open Str
    open Printf

    let except str =
      let n = String.length str - 1 in
      let b = Buffer.create 64 in
      Buffer.add_string b "\(";
      for i=0 to n do	  
	Buffer.add_string b "\(";
	for j=0 to i-1 do
	  Buffer.add_string b (quote (String.sub str j 1))
	done;
	Buffer.add_string b (sprintf "[^%s]\)" (quote (String.sub str i 1)));
	if i < n then Buffer.add_string b "\|"
      done;
      Buffer.add_string b "\)*";
      Buffer.contents b

    let checkPrefix prefix s p =
      try
	for i=0 to (String.length prefix) - 1 
	do
	  if prefix.[i] <> s.[p+i] then raise (Invalid_argument "")
	done;
	true
      with Invalid_argument _ -> false

    let skipComment start stop = 
      let pattern = regexp ((except start) ^ (quote stop)) in
      let l       = String.length start in
      (fun s p ->
	if checkPrefix start s p 
	then
	  if string_match pattern s (p+l) then p+(String.length (matched_string s))+l
	  else -1
	else p
      )

    let skipNestedComment start stop =      
      let b = regexp (quote start) in
      let n = String.length start  in
      let m = String.length stop   in
      let d = regexp (sprintf "\\(%s\\)\\|\\(%s\\)" (quote start) (quote stop)) in
      (fun s p ->
	let rec inner p =
	  if checkPrefix start s p 
	  then
	    let rec jnner p c =
	      try
		let j       = search_forward d s p in
		let nest, l = (try ignore (matched_group 1 s); true, n with Not_found -> false, m) in
		let c       = if nest then c+1 else c-1 in
		if c = 0 
		then j+l
		else jnner (j+l) c
	      with Not_found -> -1
	    in
	    jnner (p+n) 1
	  else p
	in
	inner p
      )

  end

class virtual matcher s pi coordi = 
  object (self)

    val p     = pi
    val coord = coordi

    method virtual skip : int -> Msg.Coord.t -> int * Msg.Coord.t

    method private parsed x y c = Parsed (((x, c), y), ([] : Msg.t list))
    method private failed x c   = Failed [Msg.make x [||] (Msg.Locator.Point c)]

    method get name regexp =
      let p, coord = self#skip p coord in
      if string_match regexp s p
      then 
	let m = matched_string s in
        self#parsed m {< p = p + (length m);  coord = shiftPos coord m 0 (length m) >} coord
      else self#failed (sprintf "\"%s\" expected" name) coord

    method look str = 
      let p, coord = self#skip p coord in
      try 
	let l = String.length str in
	let m = String.sub s p l in
	if str = m 
	then self#parsed m {< p = p + l; coord = shiftPos coord m 0 (length m) >} coord
	else self#failed (sprintf "\"%s\" expected" str) coord
      with Invalid_argument _ -> self#failed (sprintf "\"%s\" expected" str) coord

    method getEOF = 
      let p, coord = self#skip p coord in
      if p = length s 
      then self#parsed "<EOF>" {< p = p; coord = coord>} coord
      else self#failed "<EOF> expected" coord

    method getFIRST   = self#look   ""
    method getLAST    = self#parsed "" {<>} coord

  end

