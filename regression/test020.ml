(*
 * Test020: testing parameterised parse functions.
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

let pl = List.fold_left (^) ""

let eat s =
  (fun x ->
    LOG (printf "looking \"%s\" for \"%s\"..." (pl x) s); 
    match x with 
    | hd :: tl when hd = s -> 
	LOG (printf "ok, tail=%s\n" (pl tl)); Ok (s, fail, tl) 
    | _ -> 
	LOG (printf "fail\n"); Fail []
  )

let a = eat "A"

let _ = 
  (* <x>=a (eat x) *)
  let parse = 
    (seb 
      (fun () -> a)
      (fun x -> eat x)
    ) ()
  in
  let print = function
    | Ok ((x, y), _, s) -> 
	printf "Parsed: (%s, %s), rest: %s\n" 
	  x 
	  y
	  (pl s)

    | Fail _ -> 
	printf "Failed\n"
  in
  print (parse ["A"; "A"]);
  print (parse ["A"; "B"]);


