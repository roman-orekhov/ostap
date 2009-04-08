(*
 * Util: predefined Ostap utilities.
 * Copyright (C) 2006-2009
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

open Combinators
open Matcher 
open Printf 

module Ostap =
  struct

    module Combinators = Combinators

  end

ostap (
  listBy[delim][item]: h:item t:(-delim x:item)* {h::t}
)

ostap (
  list: listBy[ostap (",")]
)

ostap (
  list0[item]: list[item] | empty {[]}
)

type 'a assoc = ('a -> 'a -> 'a) -> ('a -> 'a) -> 'a -> 'a -> 'a

let left  f c x y = f (c x) y 
let right f c x y = c (f x y) 

let expr ops opnd =
  let ops =
    Array.map 
      (fun (assoc, list) ->
	altl (List.map (fun (oper, sema) -> ostap (!(oper) {assoc sema})) list)
      )
      ops 
  in
  let n      = Array.length ops in
  let op s i = ops.(i) s        in
  let id x   = x                in  
  let ostap (
    inner[l][c]: 
      {n = l} => x:opnd {c x}  
    | {n > l} => x:inner[l+1][id] b:(-o:op[l] inner[l][o c x])? 
      {
        match b with None -> c x | Some x -> x
      }
  )
  in 
  ostap (inner[0][id])

let read name = 
  let inch = open_in_bin name in
  let len  = in_channel_length inch in
  let buf  = String.make len ' ' in
  really_input inch buf 0 len;
  close_in inch;
  buf
