(*
 * Msg: parsing message module.
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

module Coord =
   struct

      type t = int * int

      let line = fst
      let col  = snd

      let toString (r, c) = sprintf "(%d:%d)" r c

      let next isNewline (r, c) = if isNewline then (r + 1, 1) else (r, c + 1)

      let shift coord s b n =
         let rec inner i coord =
            if i = n 
            then coord
            else inner (i+1) (next (s.[i] = '\n') coord)
         in inner b coord

      let compare (r, c) (r', c') =
         let x = compare r r' in
         if x = 0 then compare c c' else x

   end

let rec listLast = function
   | [x] -> x
   | _::t -> listLast t

module Locator =
  struct

    type t = No | Point of (string * Coord.t) | Interval of (string * Coord.t) * (string * Coord.t) | Set of t list

    let makeInterval x y =
      match x, y with
      | Point x, Point y -> Interval (x, y)
      | _ -> Set [x; y]

    let rec toString = function
      | No -> ""
      | Point (fil, coord) ->
        (match fil with "" -> "" | fil -> sprintf "%s: " fil) ^ Coord.toString coord
      | Interval ((filx, x), (fily, y)) ->
        let x, y = Coord.toString x, Coord.toString y in
        (match filx, fily with
        | "", "" -> sprintf "%s-%s" x y
        | filx, "" -> sprintf "(%s: %s)-%s" filx x y
        | "", fily -> sprintf "%s-(%s: %s)" x fily y
        | filx, fily ->
           if filx = fily
           then sprintf "%s: %s-%s" filx x y
           else sprintf "(%s: %s)-(%s: %s)" filx x fily y
        )
      | Set x ->
        let module M = View.List(struct type x = t type t = x let toString = toString end) in
        M.toString x

    let rec least = function
      | No -> invalid_arg "Locator.least No"
      | Point x -> x
      | Interval (x, _) -> x
      | Set x -> least @@ List.hd x

    let rec most = function
      | No -> invalid_arg "Locator.most No"
      | Point x -> x
      | Interval (_, y) -> y
      | Set x -> most @@ listLast x

    let unite x y =
      match (x, y) with
      | No, x
      | x, No -> x
      | x, y -> Interval (least x, most y)
      (* cba to join sets *)

    let compare x y =
      if Pervasives.compare x y = 0 then 0
      else
      match (x, y) with
      | No, No -> 0
      | No, _  -> -1
      | _ , No -> 1
      | _      ->
        let filx, x = least x and fily, y = least y in
        if Pervasives.compare filx fily = 0 then 0
        else Coord.compare x y

  end

type t = {phrase: string; args: string array; loc: Locator.t} 

let make      phrase args loc = {phrase=phrase; args=args; loc=loc}
let loc       t               = t.loc

let phrase    phrase          = make phrase [||] Locator.No
let orphan    phrase args     = make phrase args Locator.No

let string t = 
  let parmExpr = Str.regexp "%\\([0-9]+\\)" in
  Str.global_substitute 
    parmExpr  
    (fun s -> 
      try 
        t.args.(int_of_string (Str.replace_matched "\\1" s))
      with
      | Failure "int_of_string" -> 
          raise (Failure 
                   (sprintf "invalid integer parameter specification in message phrase \"%s\"" s)
                )
	    
      | Invalid_argument "index out of bounds" ->
          raise (Failure 
                   (sprintf "index out of bound while accessing message parameter in \"%s\"" s)
                )
    )
    t.phrase
    
let toString t =
  let message = string t in
    match Locator.toString t.loc with
    | ""  -> message
    | loc -> message ^ " at " ^ loc
      
let augment msg loc = match msg.loc with Locator.No -> {msg with loc = loc} | _ -> msg
let augmentList msgs loc = List.map (fun x -> augment x loc) msgs

let extend msg str = {msg with phrase=str ^ msg.phrase}
let extendList msgs str = List.map (fun msg -> extend msg str) msgs
