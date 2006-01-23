(*
 * Test019: testing bindings passing.
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
let b = eat "B"
let c = eat "C"

let _ = 
  (* <x>=a <y>=b <z>=c {x ^ y ^ z} *)
  let parse = 
    map 
      (fun (x, (y, z)) -> x ^ y ^ z) 
      ((seb 
	 (fun () -> a) 
	 (fun  x -> 
	   (seb 
	     (fun () -> b) 
	     (fun y  -> c)
	   ) ()
	 )
      ) ())
  in
  let print = function
    | Ok (x, _, s) -> 
	printf "Parsed: %s, rest: %s\n" 
	  x 
	  (pl s)

    | Fail _ -> 
	printf "Failed\n"
  in
  print (parse ["A"; "B"; "C"]);


