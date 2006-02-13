(*
 * Test010: testing basic combinators.
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
let d = function "D" :: tl -> Parsed ("D", fail, tl) | _ -> Failed []
    
let pl = List.fold_left (^) ""

let _ = 
  let parse = map (fun (x, y) -> x) (seq (iterz (seq a (iterz (seq b (iterz c))))) d) in
  let print = function
    | Parsed (x, _, s) -> 
	printf "Parsed: (%s), rest: %s\n" 
	  (match x with
	  | [] -> "None"
	  | [x, y] ->
	      sprintf 
		"Some %s, %s" x
		(match y with
		| []-> "None"
		| [b, c] -> 
		    sprintf 
		      "Some %s, %s" b
		      (match c with
		      | [] -> "None"
		      | y -> sprintf "Some %s" (pl y)
		      )
		)
	  )
	  (pl s)

    | Failed _ -> 
	printf "Failed\n"
  in
  print (parse ["A"; "D"]);
  print (parse ["A"; "B"; "D"]);
  print (parse ["A"; "B"; "C"; "D"]);


