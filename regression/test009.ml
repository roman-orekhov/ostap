(*
 * Test009: testing basic combinators.
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
    
let pl = List.fold_left (^) ""

let _ = 
  let parse = map (fun (a, ((b, c), d)) -> a, b, c, d) (seq (iterz a) (seq (seq (iterz b) a) c)) in
  let print = function
    | Ok ((x, y, z, t), _, s) -> 
	printf "Parsed: (%s, %s, %s, %s), rest: %s\n" 
	  (match x with [] -> "None" | x -> "Some " ^ (pl x)) 
	  (match y with [] -> "None" | x -> "Some " ^ (pl x)) 
	  z 
	  t
	  (pl s)

    | Fail _ -> 
	printf "Failed\n"
  in
  print (parse ["A"; "C"]);
  print (parse ["A"; "B"; "A"; "C"]);
  print (parse ["B"; "A"; "C"]);
  print (parse ["A"; "A"; "C"]);


