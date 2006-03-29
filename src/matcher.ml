(*
 * Matcher: simple lexer pattern.
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

class virtual ['a] matcher (make : string -> int -> Msg.Coord.t -> 'a) s p coord = 
  object (self)

    method virtual skip : int * Msg.Coord.t

    method get (name : string) regexp =
      LOG (printf "Trying %s at %s\n" name (sub s p (min 10 (length s - p)))); 
      let p, coord = self#skip in
      if string_match regexp s p
      then begin
	let m = matched_string s in
	LOG (printf "Ok, repr=%s\n" m);
	let p = p + length m in	
	Parsed ((m, coord), make s p (shiftPos coord m 0 (length m)))
      end
      else 
	Failed [Msg.make (sprintf "%s expected" name) [||] (Msg.Locator.Point coord)]

    method look str = 
      let p, coord = self#skip in
      try 
	let l = String.length str in
	let m = String.sub s p l in
	if str = m 
	then Parsed ((m, coord), make s (p+l) (shiftPos coord m 0 (length m)))
	else Failed [Msg.make (sprintf "%s expected" str) [||] (Msg.Locator.Point coord)]
      with Invalid_argument _ -> Failed [Msg.make (sprintf "%s expected" str) [||] (Msg.Locator.Point coord)]

    method getEOF = 
      let p, coord = self#skip in
      LOG (printf "Trying <EOF> at %s\n" (sub s p (min 10 (length s - p)))); 
      if p = length s 
      then Parsed (("<EOF>", coord), make s p coord)
      else Failed [Msg.make "<EOF> expected" [||] (Msg.Locator.Point coord)]

    method getFIRST   = self#look ""
    method getLAST    = 
      (
       Parsed (("", coord), make s p coord) : 
	 ((string * Msg.Coord.t) * 'a, Msg.t) Ostap.tag
      )

  end

