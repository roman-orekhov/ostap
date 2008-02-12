(*
 * Ostap: basic set of parser combinators.
 * Copyright (C) 2006-2008
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

open Printf
open List

type ('a, 'b) tag = Parsed of 'a * 'b list | Error of 'b list | Failed of 'b list

type ('a, 'b, 'c) result = ('b * 'a, 'c) tag
and  ('a, 'b, 'c) parse  = 'a -> ('a, 'b, 'c) result
      
let return x = (fun s -> Parsed ((x, s), []))
let cast     = function Error x -> Error x | Failed x -> Failed x | _ -> invalid_arg "Ostap.cast"

let map f p = 
  (fun s ->
    match p s with
    | Parsed ((b, s'), e) -> Parsed ((f b, s'), e)
    | x -> cast x
  )

let (-->) p f = map f p

let empty s = Parsed (((), s), [])
let rise  s = Parsed ((s, s), [])

let alt x y =
  (fun s ->
    LOG (printf "running alt\n");
    match x s with 
    | Error x -> 
	LOG (printf "alt left part error, trying its right part\n"); 
	begin match y s with 
	| Error y | Failed y -> Error (x @ y) 
	| Parsed (ok, err) -> Parsed (ok, x @ err)
	end
	  
    | Failed x -> 
	LOG (printf "alt left part failed, trying its right part\n"); 
	y s 
	  
    | x -> 
	LOG (printf "alt left part parsed\n"); 
	x
  )
    
let (<|>) = alt

let seq x y =
  (fun s -> 
    LOG (printf "running seq\n");
    match x s with
    | Parsed ((b, s'), err) ->	
	LOG (printf "seq right part parsed, trying its left part\n");
	begin match y b s' with 
	| Failed x | Error x -> Error (err @ x) 
	| x -> x
	end

    | x -> 
	LOG (printf "seq left part failed\n");
	cast x
  )
    
let (|>) = seq

let opt p = p --> (fun x -> Some x) <|> return None

let (<?>) = opt
    
let rec many p = 
  (fun s ->
    ((p |> (fun h -> many p --> (fun t -> h :: t))) <|> return []) s
  )
  
let (<*>) = many

let some p = (p |> (fun h -> many p --> (fun t -> h :: t)))

let (<+>) = some
    
let guard p f = 
  (fun s ->
    match p s with
    | (Parsed ((b, _), _) as x) -> if f b then x else Failed []
    | y -> y
  )

(*
let comment p str =
  (fun s ->
    match p s with
    | (Parsed _ as x) -> x
    | Failed 
  )
*)
