(*
 * Ostap: basic set of parser combinators.
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

open Printf
open List

type ('a, 'b) tag = Parsed of 'a | Error of 'b list | Failed of 'b list

type ('a, 'b, 'c) result = ('b * ('a, 'b, 'c) parse * 'a, 'c) tag
and  ('a, 'b, 'c) parse  = 'a -> ('a, 'b, 'c) result
      
let fail _ = Failed []

let cut x =
  (fun s ->
    match x s with
    | Parsed (b, rest, s') -> Parsed (b, fail, s')
    | y -> y 
  )

let rec alt x y =
  (fun s ->
    LOG (printf "running alt\n");
    match x s with
    | Parsed (b, rest, s') -> 
	LOG (printf "alt left part okeyed\n"); 
	Parsed (b, alt rest y, s') 
	  
    |  _ -> 
	LOG (printf "alt left part failed, trying its right part\n"); 
	y s
  )

let (<!>) = alt

let alc x y = cut (alt x y)

let (<|>) = alc

let rec seq x y =
  (fun s -> 
    LOG (printf "running seq\n");
    match x s with
    | Parsed (b, rest, s') -> 
	LOG (printf "seq's left part okeyed, trying its right part\n");
	begin match y s' with
	| Parsed (b', rest', s'') -> 
	    LOG (printf "seq's right part okeyed\n"); 
	    Parsed 
	      (
	       (b, b'), 
	       alt (seq rest y) (seq x rest'), 
	       s''
	      )
	|  _ -> 
	    LOG (printf "seq's right part failed, running seq rest\n"); 
	    seq rest y s
	end
	  
    | Failed x -> Failed x
    | Error  x -> Error  x
  )

let (|!>) = seq
  
let rec seb x y =
  (fun s -> 
    match x s with
    | Parsed (b, rest, s') -> 
	begin match y b s' with
	| Parsed (b', rest', s'') -> 
	    Parsed 
	      (
	       (b, b'), 
	       alt (seb rest y) (seb x (fun _ -> rest')), 
	       s''
	      )
	      
	| _ -> (seb rest y) s
	end
	  
    | Failed x -> Failed x
    | Error  x -> Error  x
  )

let (||!>) = seb

let rec sec x y =
  (fun s -> 
    LOG (printf "running sec\n");
    match x s with
    | Parsed (b, rest, s') -> 
	LOG (printf "sec's left part okeyed, trying its right part\n");
	begin match y s' with
	| Parsed (b', rest', s'') -> 
	    LOG (printf "sec's right part okeyed\n"); 
	    Parsed 
	      (
	       (b, b'), 
	       alc (seq rest y) (seq x rest'), 
	       s''
	      )
	| Failed x | Error x -> 
	    LOG (printf "sec's right part failed, running seq rest\n"); 
	    Error x
	end
	  
    | Failed x -> Failed x
    | Error  x -> Error  x
  )

let (|>) = sec
  
let rec secb x y =
  (fun s -> 
    match x s with
    | Parsed (b, rest, s') -> 
	begin match y b s' with
	| Parsed (b', rest', s'') -> 
	    Parsed 
	      (
	       (b, b'), 
	       alc (secb rest y) (secb x (fun _ -> rest')), 
	       s''
	      )
	      
	| Failed x | Error x -> Error x
	end
	  
    | Failed x -> Failed x
    | Error  x -> Error  x
  )

let (||>) = secb

let rec opt x =
  (fun s ->
    LOG (printf "running opt\n");
    match x s with
    | Parsed (b, rest, s') -> 
	LOG (printf "opt okeyed\n"); 
	Parsed (Some b, opt rest, s')

    | Failed x -> 
	LOG (printf "opt failed, None returned\n"); 
	Parsed (None, (fun _ -> Failed x), s)

    | Error x ->
	LOG (printf "opt failed, None returned\n"); 
	Parsed (None, (fun _ -> Error x), s)

  )    

let (<?>) = opt

let rec map f = 
  (fun p ->
    (fun s ->
      match p s with
      | Parsed (b, rest, s') -> Parsed (f b, (map f rest), s')
      | Failed x -> Failed x
      | Error  x -> Error  x
    )
  )

let rec iterz  =
  (** common sub-expression*)
  let common p = map (function None -> [] | Some (hd, tail) -> hd :: tail)  
                  (opt p)
  in
  fun x ->
  (fun s ->
    LOG (printf "running iterz\n");
    match x s with
    | Parsed (b, rest, s') -> 
	LOG (printf "iterz's x okeyed\n");
        begin match iterz x s' with
	| Parsed (tail, rest', s'') ->
	    Parsed 
	      (
	       b :: tail, 
	       common 
		    (alt   (seq x    rest') 
			      (seq rest (iterz x))
 		    ), 
	       s''
	      )
	| Failed x -> Parsed ([b], iterz rest, s')
	| Error  x -> Error  x
	end

   | Failed _ -> 
       LOG (printf "iterz's x failed, returning []\n"); 
       Parsed ([], fail, s) 

   | Error x ->
       LOG (printf "iterz's x error reported, returning error\n");
       Error x
  )

let (<*>) = iterz

let iter x = map (fun (hd, tl) -> hd :: tl) (seq x (iterz x))

let (<+>) = iter

let guard p f = 
  (fun s ->
    match p s with
    | (Parsed (b, rest, s') as x) -> if f b then x else Failed []
    | y -> y
  )
