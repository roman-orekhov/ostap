(*
 * Test001: testing basic combinators.
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

let a = function "A" :: tl -> Parsed ("A", tl) | _ -> Failed []
let b = function "B" :: tl -> Parsed ("B", tl) | _ -> Failed []
let c = function "C" :: tl -> Parsed ("C", tl) | _ -> Failed []
    
let pl = List.fold_left (^) ""

let _ = 
  let parse = seq_bt (alt_bt a b) (alt_bt b c) in
  match parse ["A"; "C"] with
  | Parsed ((_, rest), _) ->
      printf "AC - parsed\n";
      begin match rest ["B"; "B"] with
      | Parsed ((_, rest), _) -> 
	  printf "BB - parsed\n";
      | _ -> printf "BB - failed\n"
      end;
      begin match rest ["A"; "B"] with
      | Parsed ((_, rest), _) -> printf "AB - parsed\n"
      | _ -> printf "AB - failed\n"
      end;
      begin match rest ["B"; "C"] with
      | Parsed ((_, rest), _) -> printf "BC - parsed\n"
      | _ -> printf "BC - failed\n"
      end;

  | Failed _ -> printf "AC - failed\n"
