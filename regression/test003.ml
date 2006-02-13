(*
 * Test003: testing basic combinators.
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
open Printf

let a = function "A" :: tl -> Parsed ("A", fail, tl) | _ -> Failed []
let b = function "B" :: tl -> Parsed ("B", fail, tl) | _ -> Failed []
let c = function "C" :: tl -> Parsed ("C", fail, tl) | _ -> Failed []
    
let pl = List.fold_left (^) ""

let _ = 
  let parse = map (fun (x, (y, z)) -> x, y, z) (seq a (seq b c)) in
  let print = function
    | Parsed ((x, y, z), _, s) -> 
	printf "Parsed: (%s, %s, %s), rest: %s\n" x y z (pl s)

    | Failed _ -> 
	printf "Failed\n"
  in
  print (parse ["B"]);
  print (parse ["A"; "B"; "C"]);
  print (parse ["A"; "B"]);

