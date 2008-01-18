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

