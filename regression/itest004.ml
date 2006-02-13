(*
 * Itest004: testing basic combinators in infix notation.
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

let pl = List.fold_left (^) ""

let eat s =
  (fun x ->
    LOG (printf "looking \"%s\" for \"%s\"..." (pl x) s); 
    match x with 
    | hd :: tl when hd = s -> 
	LOG (printf "ok, tail=%s\n" (pl tl)); 
	Parsed ("A", fail, tl) 

    | _ -> 
	LOG (printf "fail\n"); 
	Failed []
  )

let a = eat "A"
let b = eat "B"
let c = eat "C"

let _ = 
  let parse = map (fun ((a, (b, c)), d) -> a, b, c, d) ((<+>) a |!> ((<?>) b |!> a) |!> c) in
  let print = function
    | Parsed ((x, y, z, t), _, s) -> 
	printf "Parsed: (%s, %s, %s, %s), rest: %s\n" 
	  (pl x) 
	  (match y with None -> "None" | Some x -> "Some " ^ x) 
	  z 
	  t
	  (pl s)

    | Failed _ -> 
	printf "Failed\n"
  in
(*  print (parse ["A"; "C"]);
  print (parse ["B"; "A"; "C"]);*)
  print (parse ["A"; "A"; "C"]);
(*  print (parse ["A"; "B"; "A"; "C"]);
  print (parse ["A"; "A"; "A"; "B"; "A"; "C"]);
  print (parse ["A"; "A"; "A"; "A"; "C"]);*)


