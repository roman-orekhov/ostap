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

type 'a t = [ `EOS | `Cons of 'a * 'a t ] Lazy.t

let guarded f = try f () with End_of_file -> `EOS
let defer   f = Lazy.lazy_from_fun (fun () -> guarded f)

let rec fromFunction  f      = defer (fun _ -> `Cons (f (), fromFunction f))
let rec fromChannel   f inch = defer (fun _ -> `Cons (f inch, fromChannel f inch))
let rec fromIterator  x f    = defer (fun _ -> let y, x' = f x in `Cons (y, fromIterator x' f))
let rec fromGenerator x n i  = defer (fun _ -> `Cons (i x, fromGenerator (n x) n i))
let rec fromList      l      = fromIterator  l (function [] -> raise End_of_file, [] | hd::tl -> hd, tl)
let rec fromArray     a      = let n = Array.length a in fromGenerator 0 (fun i -> i+1) (fun i -> if i < n then a.(i) else raise End_of_file)
let     fromFile             = fromChannel input_char

let get   s = Lazy.force s
let endOf s = get s = `EOS

let elem  s = match get s with `EOS -> raise End_of_file | `Cons (x, _) -> x

let rec map  f s = Lazy.lazy_from_val (match get s with `EOS -> `EOS | `Cons (x, y) -> `Cons (f x, map f y))
let rec iter f s = match get s with `EOS -> () | `Cons (x, y) -> f x; iter f y

let rec fold f x s = match get s with `EOS -> x | `Cons (z, y) -> fold f (f x z) y

let rec filter f s = Lazy.lazy_from_val (match get s with `EOS -> `EOS | `Cons (x, y) as z -> if f x then get (filter f y) else z) 

let rec zip x y = Lazy.lazy_from_val (match get x, get y with `EOS, _ | _, `EOS -> `EOS | `Cons (x, xl), `Cons (y, yl) -> `Cons ((x, y), zip xl yl))

let rec zip3 x y z = 
  Lazy.lazy_from_val (
    match get x, get y, get z with 
    | `EOS, _, _ | _, `EOS, _ | _, _, `EOS -> `EOS 
    | `Cons (x, xl), `Cons (y, yl), `Cons (z, zl) -> `Cons ((x, y, z), zip3 xl yl zl)
  )

let rec zip4 x y z t = 
  Lazy.lazy_from_val (
    match get x, get y, get z, get t with 
    | `EOS, _, _, _ | _, `EOS, _, _ | _, _, `EOS, _ | _, _, _, `EOS -> `EOS 
    | `Cons (x, xl), `Cons (y, yl), `Cons (z, zl), `Cons (t, tl) -> `Cons ((x, y, z, t), zip4 xl yl zl tl)
  )
