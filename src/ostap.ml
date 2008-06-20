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

type ('a, 'b) tag = Parsed of 'a * 'b option | Failed of 'b option

type ('a, 'b, 'c) result = ('b * 'a, 'c) tag
and  ('a, 'b, 'c) parse  = 'a -> ('a, 'b, 'c) result

let join = function
  | None   -> fun y -> y
  | Some x -> function None -> Some x | Some y -> Some (x#add y)

let comment str = function
  | None   -> None
  | Some m -> Some (m#comment str)

let return x = (fun s -> Parsed ((x, s), None))
let cast     = function Failed x -> Failed x | _ -> invalid_arg "Ostap.cast"

let map f p s = 
  match p s with
  | Parsed ((b, s'), e) -> Parsed ((f b, s'), e)
  | x -> cast x

let (-->) p f = map f p

let empty s = Parsed (((), s), None)
let fail  s = Failed None
let lift  s = Parsed ((s, s), None)

let alt x y s =
  match x s with 
  | Failed x -> 	
      (match y s with 
      | Failed y -> Failed (join x y) 
      | Parsed (ok, err) -> Parsed (ok, join x err)
      )
  | x -> x
    
let (<|>) = alt

let seq x y s =
  match x s with
  | Parsed ((b, s'), err) ->	
      (match y b s' with 
      | Failed x -> Failed (join err x) 
      | x -> x
      )	
  | x -> cast x
    
let (|>) = seq

let opt p = p --> (fun x -> Some x) <|> return None

let (<?>) = opt
    
let rec many p s = ((p |> (fun h -> many p --> (fun t -> h :: t))) <|> return []) s
  
let (<*>) = many

let some p = (p |> (fun h -> many p --> (fun t -> h :: t)))

let (<+>) = some
    
let guard p f s = 
  match p s with
  | (Parsed ((b, _), _) as x) -> if f b then x else Failed None
  | y -> y

let comment p str s =
  match p s with
  | (Parsed _ as x) -> x
  | Failed m -> Failed (comment str m)
