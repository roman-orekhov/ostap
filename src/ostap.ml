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
open Checked

type ('a, 'b, 'c) result = ('b * ('a, 'b, 'c) parse * 'a, 'c) Checked.t
and  ('a, 'b, 'c) parse  = 'a -> ('a, 'b, 'c) result
      
let fail _ = Fail []

let cut x =
  (fun s ->
    match x s with
    | Ok (b, rest, s') -> Ok (b, fail, s')
    | y -> y 
  )

let rec alt x y =
  (fun s ->
    LOG (printf "running alt\n");
    match x s with
    | Ok (b, rest, s') -> 
	LOG (printf "alt left part okeyed\n"); 
	Ok (b, alt rest y, s') 
	  
    | Fail _ -> 
	LOG (printf "alt left part failed, trying its right part\n"); 
	y s
  )

let (<!>) = alt

let alp x y = cut (alt x y)

let (<|>) = alp

let rec seq x y =
  (fun s -> 
    LOG (printf "running seq\n");
    match x s with
    | Ok (b, rest, s') -> 
	LOG (printf "seq's left part okeyed, trying its right part\n");
	begin match y s' with
	| Ok (b', rest', s'') -> 
	    LOG (printf "seq's right part okeyed\n"); 
	    Ok 
	      (
	       (b, b'), 
	       alt (seq rest y) (seq x rest'), 
	       s''
	      )
	| Fail _ -> 
	    LOG (printf "seq's right part failed, running seq rest\n"); 
	    seq rest y s
	end
	  
    | Fail x -> Fail x
  )

let (|!>) = seq
  
let rec seb x y =
  (fun context ->
    (fun s -> 
      match x context s with
      | Ok (b, rest, s') -> 
	  begin match y b s' with
	  | Ok (b', rest', s'') -> 
	      Ok 
		(
		 (b, b'), 
		 alt ((seb (fun _ -> rest) y) context) ((seb x (fun _ -> rest')) context), 
		 s''
		)

	  | Fail _ -> (seb (fun _ -> rest) y) context s
	  end
	    
      | Fail x -> Fail x
    )
  )

let (||!>) = seb

let rec sec x y =
  (fun s -> 
    LOG (printf "running sec\n");
    match x s with
    | Ok (b, rest, s') -> 
	LOG (printf "sec's left part okeyed, trying its right part\n");
	begin match y s' with
	| Ok (b', rest', s'') -> 
	    LOG (printf "sec's right part okeyed\n"); 
	    Ok 
	      (
	       (b, b'), 
	       alp (seq rest y) (seq x rest'), 
	       s''
	      )
	| Fail x -> 
	    LOG (printf "sec's right part failed, running seq rest\n"); 
	    Fail x
	end
	  
    | Fail x -> Fail x
  )

let (|>) = sec
  
let rec secb x y =
  (fun context ->
    (fun s -> 
      match x context s with
      | Ok (b, rest, s') -> 
	  begin match y b s' with
	  | Ok (b', rest', s'') -> 
	      Ok 
		(
		 (b, b'), 
		 alp ((secb (fun _ -> rest) y) context) ((secb x (fun _ -> rest')) context), 
		 s''
		)

	  | Fail x -> Fail x
	  end
	    
      | Fail x -> Fail x
    )
  )

let (||>) = secb

let rec opt x =
  (fun s ->
    LOG (printf "running opt\n");
    match x s with
    | Ok (b, rest, s') -> 
	LOG (printf "opt okeyed\n"); 
	Ok (Some b, opt rest, s')

    | Fail x -> 
	LOG (printf "opt failed, None returned\n"); 
	Ok (None, (fun _ -> Fail x), s)
  )    

let (<?>) = opt

let rec map f = 
  (fun p ->
    (fun s ->
      match p s with
      | Ok (b, rest, s') -> Ok (f b, (map f rest), s')
      | Fail x -> Fail x
    )
  )

let rec iterz x =
  (fun s ->
    LOG (printf "running iterz\n");
    match x s with
    | Ok (b, rest, s') -> 
	LOG (printf "iterz's x okeyed\n");
	let Ok (tail, rest', s'') = iterz x s' in
	Ok 
	  (
	   b :: tail, 
	   (alt 
	      (map 
		 (function None -> [] | Some (hd, tail) -> hd :: tail) 
		 (opt (seq x rest'))
	      )	      
	      (alt
		 (map 
		    (function None -> [] | Some (hd, tail) -> hd :: tail) 
		    (opt (seq rest rest'))
		 )	       
		 (map 
		    (function None -> [] | Some (hd, tail) -> hd :: tail) 
		    (opt (seq rest (iterz x)))
		 )
	      )
	   ), 
	   s''
	  )

   | Fail x -> 
       LOG (printf "iterz's x failed, returning []\n"); 
       Ok ([], fail, s) 
  )

let (<*>) = iterz

let iter x = map (fun (hd, tl) -> hd :: tl) (seq x (iterz x))

let (<+>) = iter

let guard f p = (fun x -> if f x then p else fail)
