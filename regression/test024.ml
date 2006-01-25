(*
 * Test014: testing basic combinators.
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

let a = function "A" :: tl -> Ok ("A", fail, tl) | _ -> Fail []
let b = function "B" :: tl -> Ok ("B", fail, tl) | _ -> Fail []
let c = function "C" :: tl -> Ok ("C", fail, tl) | _ -> Fail []
let d = function "D" :: tl -> Ok ("D", fail, tl) | _ -> Fail []
    
let pl = List.fold_left (^) ""

let _ = 
  let parse = seq (opt (alp a (alp b c))) d in
  let print = function
    | Ok ((x, y), _, s) -> 
	printf "Parsed: (%s, %s), rest: %s\n" 
	  (match x with None -> "None" | Some x -> "Some " ^ x) 
	  y
	  (pl s)

    | Fail _ -> 
	printf "Failed\n"
  in
  print (parse ["D"]);
  print (parse ["C"; "D"]);
  print (parse ["A"; "D"]);
  print (parse ["B"; "D"]);
