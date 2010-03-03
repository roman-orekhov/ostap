(*
 * Stream: lazy lists.
 * Copyright (C) 2006-2010
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

open Lazy

type 'a t = ('a * 'a t) Lazy.t

let rec fromFunction  f      = lazy_from_fun (fun _ -> f (), fromFunction f)
let rec fromChannel   f inch = lazy_from_fun (fun _ -> f inch, fromChannel f inch)
let rec fromIterator  x f    = lazy_from_fun (fun _ -> let y, x' = f x in y, fromIterator x' f)
let rec fromGenerator x n i  = lazy_from_fun (fun _ -> i x, fromGenerator (n x) n i)
let rec fromList      l      = fromIterator  l (function [] -> raise End_of_file, [] | hd::tl -> hd, tl)
let rec fromArray     a      = let n = Array.length a in fromGenerator 0 (fun i -> i+1) (fun i -> if i < n then a.(i) else raise End_of_file)
let     fromFile             = fromChannel input_char

let complete f x = try f () with End_of_file -> x

let get   s = force s
let endOf s = complete (fun _ -> ignore (get s); false) true 
let hd    s = fst (get s)
let tl    s = snd (get s)

let rec map  f s   = lazy_from_fun (fun _ -> let x, y = get s in f x, map f y)
let rec iter f s   = complete (fun _ -> let x, y = get s in f x; iter f y) ()
let rec fold f x s = complete (fun _ -> let z, y = get s in fold f (f x z) y) x
let rec filter f s = lazy_from_fun (fun _ -> let (x, y) as z = get s in if f x then get (filter f y) else z)

let rec zip x y      = lazy_from_fun (fun _ -> let (x, xl), (y, yl)                   = get x, get y               in (x, y)      , zip  xl yl      )
let rec zip3 x y z   = lazy_from_fun (fun _ -> let (x, xl), (y, yl), (z, zl)          = get x, get y, get z        in (x, y, z)   , zip3 xl yl zl   )
let rec zip4 x y z t = lazy_from_fun (fun _ -> let (x, xl), (y, yl), (z, zl), (t, tl) = get x, get y, get z, get t in (x, y, z, t), zip4 xl yl zl tl)