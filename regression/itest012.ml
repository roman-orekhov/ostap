(*
 * Itest012: testing basic combinators in infix notation.
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
  let parse = map (fun ((a, l), (b, d)) -> a :: l, b, d) ((a |!> (<*>) a) |!> ((<?>) b |!> a)) in
  let print = function
    | Parsed ((x, y, z), _, s) -> 
	printf "Parsed: (%s, %s, %s), rest: %s\n" 
	  (pl x) 
	  (match y with None -> "None" | Some x -> "Some " ^ x) 
	  z 
	  (pl s)

    | Failed _ -> 
	printf "Failed\n"
  in
  print (parse ["A"; "A"]);
  print (parse ["A"; "B"; "A"]);
  print (parse ["A"; "A"; "B"; "A"]);



