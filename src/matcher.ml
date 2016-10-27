(*
 * Matcher: simple lexer pattern.
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

open Combinators
open String
open Printf
open Str
        
module Token =
  struct

    type t = string * Msg.Locator.t

    let toString (t, l) = sprintf "%s at %s" t (Msg.Locator.toString l)

    let loc  = snd
    let repr = fst

  end

let except str =
  let n = String.length str - 1 in
  let b = Buffer.create 64 in
  Buffer.add_string b "\(";
  for i=0 to n do          
    Buffer.add_string b "\(";
    for j=0 to i-1 do
      Buffer.add_string b (quote (String.sub str j 1))
    done;
    Buffer.add_string b (sprintf "[^%s]\)" (quote (String.sub str i 1)));
    if i < n then Buffer.add_string b "\|"
  done;
  Buffer.add_string b "\)*";
  Buffer.contents b
    
let checkPrefix prefix s p =
  try
    for i=0 to (String.length prefix) - 1 
    do
      if prefix.[i] <> s.[p+i] then raise (Invalid_argument "")
    done;
    true
  with Invalid_argument _ -> false
      
module Skip =
  struct

    type t = string -> int -> [`Skipped of int | `Failed of string] 

    let comment start stop = 
      let pattern = regexp ((except start) ^ (quote stop)) in
      let l       = String.length start in
      (fun s p ->
        if checkPrefix start s p 
        then
          if string_match pattern s (p+l) then `Skipped (p+(String.length (matched_string s))+l)
          else `Failed (sprintf "unterminated comment ('%s' not detected)" stop)
        else `Skipped p
      )
    
    let nestedComment start stop =      
      let n = String.length start  in
      let m = String.length stop   in
      let d = regexp (sprintf "\\(%s\\)\\|\\(%s\\)" (quote start) (quote stop)) in
      (fun s p ->
        let rec inner p =
          if checkPrefix start s p 
          then
            let rec jnner p c =
              try
                let j       = search_forward d s p in
                let nest, l = (try ignore (matched_group 1 s); true, n with Not_found -> false, m) in
                let c       = if nest then c+1 else c-1 in
                if c = 0 
                then `Skipped (j+l)
                else jnner (j+l) c
              with Not_found -> `Failed (sprintf "unterminated comment ('%s' not detected)" stop)
            in
            jnner (p+n) 1
          else `Skipped p
        in
        inner p
      )
        
    let lineComment start =
      let e = regexp ".*$" in
      let n = String.length start in
      (fun s p ->
        if checkPrefix start s p 
        then
          if string_match e s (p+n)
          then `Skipped (p+n+(String.length (matched_string s)))
          else `Skipped (String.length s)
        else `Skipped p
      )
        
    let whitespaces symbols =
      let e = regexp (sprintf "[%s]*" (quote symbols)) in
      (fun s p ->
        try 
          if string_match e s p 
          then `Skipped (p+(String.length (matched_string s)))
          else `Skipped p  
        with Not_found -> `Skipped p
      )

    let rec create skippers = 
      let f =
        List.fold_left 
          (fun acc g ->
            (fun s p ->
              match acc s p with
              | `Skipped p -> g s p
              | x -> x
            )
          )
          (fun s p -> `Skipped p)
          skippers
      in
      (fun s p coord ->
        let rec iterate s p =
          match f s p with
          | (`Skipped p') as x when p = p' -> x
          | `Skipped p' -> iterate s p'
          | x -> x
        in
        match iterate s p with
        | `Skipped p' -> `Skipped (p', Msg.Coord.shift coord s p p')
        | `Failed msg -> `Failed msg
      )

  end

module Errors =
   struct
      type t =
         | Deleted  of   char * int * Msg.Coord.t * strings
         | Inserted of string * int * Msg.Coord.t * strings

      module S = Set.Make(Compare.String)

      let getCoord = function
      | Deleted  (_, _, coord, _)
      | Inserted (_, _, coord, _) -> coord
      
      let expected sort strings =
         let l = if sort
            then S.elements (List.fold_left (fun acc cur -> S.add cur acc) S.empty strings)
            else strings in
         sprintf "expected [%s]" (String.concat ", " l)

      let toExpected sort = function
      | Deleted  (_, _, _, strings)
      | Inserted (_, _, _, strings) -> expected sort strings

      let toAction = function
      | Deleted  (c, _, _, _) -> sprintf "deleted  '%c'" c
      | Inserted (s, _, _, _) -> sprintf "inserted %s" s

      let toMsg withAction toLocator e = Msg.make (if withAction then toAction e else toExpected true e) [||] (toLocator (getCoord e))

      let toMsgFull toLocator e = Msg.make (sprintf "%s %s => %s" (Msg.Coord.toString @@ getCoord e) (toExpected false e) (toAction e)) [||] (toLocator (getCoord e))

      let filter maxInRow correctRows errors =
         let byRow, last, _, _, lastNum =
         List.fold_left (fun (res, acc, prevRow, prevCol, num) cur ->
            let row, col = getCoord cur in
            if row != prevRow
            then ((num,acc)::res, [cur], row, col, 1)
            else let acc = if num+1 > maxInRow then [List.hd acc] else acc@[cur] in
               (res, acc, row, col, num+1)
         ) ([], [], 0, 0, 0) errors in
         (* first in byRow will be (0, []), remove it *)
         let _::byRow = List.rev ((lastNum,last)::byRow) in
         let res, last, _, _, _ = 
         List.fold_left (fun (res, firstErrors, firstRow, prevRow, num) (inRow, errs) ->
            let row, _ = getCoord (List.hd errs) in
            let rows = row - firstRow + 1 in
            if rows < num + inRow && row - prevRow <= correctRows (* too much errors in one row - distant rows are considered a part of those errors - prohibit this *)
            then (res, [true, snd (List.hd firstErrors)], firstRow, row, num+inRow)
            else (List.rev_append firstErrors res, List.map (fun e -> inRow > maxInRow, e) errs, row, row, inRow)
         )
         ([], [], 0, 0, 0) byRow
         in List.rev_append res last

      let correct s errors =
         let buf = Buffer.create 1024 in
         let rec inner i = function
         | [] -> Buffer.add_substring buf s i (String.length s-i); Buffer.contents buf
         | h::t -> match h with
            | Deleted  (_, j, _, _) -> Buffer.add_substring buf s i (j-i); inner (j+1) t
            | Inserted (ins, j, _, _) -> Buffer.add_substring buf s i (j-i); Buffer.add_string buf ins; inner j t
         in inner 0 errors
   end

class type ['b] m =
  object ('a)
    method pos : int
    method coord : Msg.Coord.t
    method line : int
    method col : int
    method errors : Errors.t list
    method prefix : int -> string
    method pFuncCost : (int -> string option) * string * string -> [ `Exact of int | `Length ] -> ('a, Token.t, 'b) parsed
    method get : ?except:(string -> bool) -> string -> Str.regexp -> string -> ('a, Token.t, 'b) parsed
    method regexp : ?except:(string -> bool) -> string -> string -> string -> ('a, Token.t, 'b) parsed
    method getEOF : ('a, Token.t, 'b) parsed
    method loc : Msg.Locator.t
    method reloc : Msg.Coord.t -> Msg.Locator.t
    method look : string -> ('a, Token.t, 'b) parsed
    method skip : int -> Msg.Coord.t -> [`Skipped of int * Msg.Coord.t | `Failed of string]
  end

type aux = [`Skipped of int * Msg.Coord.t | `Failed of string | `Init]

let defaultSkipper = fun (p : int) (c : Msg.Coord.t) -> (`Skipped (p, c) :> [`Skipped of int * Msg.Coord.t | `Failed of string])

class ['b] t ?name:(n = "") ?relocs:(r = Relocs.MC.empty) s =   
  let locate r c = try Relocs.getSuccReloc s r c with _ -> r, (n, c) in
  object (self : 'b #m)
    val regexps = Hashtbl.create 256
    val p       = 0
    val coord   = (1, 1)
    val skipper = defaultSkipper
    val context : aux = `Init
    val del_ok  = true
    val errors  = []
  
    method reloc coord = Msg.Locator.Point (snd @@ locate r coord)
    method skip = skipper
    method private changeSkip sk =
      let newContext =
      match context with
      | `Failed msg -> `Failed msg
      | `Init -> ((sk p coord) :> aux)
      | `Skipped (p, coord) -> ((sk p coord) :> aux)
      in {< skipper = sk; context = newContext >}

 
    method pos   = p
    method coord = coord
    method line  = Msg.Coord.line coord
    method col   = Msg.Coord.col coord
    method errors= List.rev errors

    method prefix n =
      if p + n < String.length s 
      then String.sub s p n
      else String.sub s p (String.length s - p)

    method regexp ?except name str = self#get ?except name 
      (try Hashtbl.find regexps str with Not_found ->
         let regexp = Str.regexp str in
         Hashtbl.add regexps str regexp;
         regexp 
      )

   method pFuncCost (f, msg, min_def) cost =
      let p, coord = match context with 
         | `Failed msg -> p, coord
         | `Init -> (match self#skip p coord with
            | `Skipped (p, coord) -> p, coord
            | `Failed msg -> p, coord
         )
         | `Skipped (p, coord) -> p, coord in
      let l = String.length min_def in
      let getCost = function
         | `Exact cst -> cst
         | `Length -> 5 * l in
      let inner k =
         match f p with
         | Some m ->
            if !Combinators.debug then printf "found \"%s\" while looking for %s\n" m msg;
            let l = String.length m in
            let p = p + l
            and c = Msg.Coord.shift coord m 0 l in
            let succ, x = locate r coord in
            let _, y = locate succ c in
            Ostream.consL
            (Step l)
            (lazy (k (m, Msg.Locator.Interval (x, y)) {< p = p; coord = c; context = ((self#skip p c) :> aux); del_ok = true >}))
         | None ->
            if !Combinators.debug then printf "couldn't find %s\n" msg;
            Ostream.one (Fail (lazy ([msg],
               (let ins = (getCost cost, false, fun exp -> 
                  let next = {< errors = (Errors.Inserted (msg, p, coord, exp))::errors; del_ok = false >} in
                  if !Combinators.debug then printf "Inserted \"%s\"\n%s\n" min_def (Errors.correct s next#errors);
                  k (min_def, self#reloc coord) next) in
               if not del_ok || p = String.length s
               then [ins]
               else let del = (5, true, fun exp ->
                  let np = p + 1 in
                  let c = Msg.Coord.shift coord s p np in
                  let next = {< p=np; coord=c; context=((self#skip np c) :> aux); del_ok = true; errors = (Errors.Deleted (s.[p], p, coord, exp))::errors >} in
                  if !Combinators.debug then printf "Deleted \"%c\"\n%s\n" s.[p] (Errors.correct s next#errors);
                  next#pFuncCost (f, msg, min_def) cost k )
               in [ins;del])
            )))
            
      in inner

    method get ?except name regexp min =
      self#pFuncCost 
      ((fun p ->
         if string_match regexp s p
         then
            let res = matched_string s in
            match except with
            | Some f -> if f res then None else Some res
            | None -> Some res
         else None
      ), sprintf "<%s>" name, min) (`Exact 5)
      (*`Length*)

    method look str =
      self#pFuncCost 
      ((fun p ->
         if checkPrefix str s p
         then Some str
         else None
      ), sprintf "\"%s\"" str, str) (`Exact 5)
      (*`Length*)

   method getEOF k =
      let p, coord = match context with 
         | `Failed msg -> p, coord
         | `Init -> (match self#skip p coord with
            | `Skipped (p, coord) -> p, coord
            | `Failed msg -> p, coord
         )
         | `Skipped (p, coord) -> p, coord in
      if p = length s
      then begin
         if !Combinators.debug then printf "found EOF while looking for EOF\n";
         Ostream.consL
         (Step 0)
         (lazy (k ("EOF", self#reloc coord) {< p = p; coord = coord; del_ok = false >}))
      end else begin
         if !Combinators.debug then printf "couldn't find EOF\n";
         Ostream.one (Fail (lazy (["EOF"],
               if not del_ok
               then []
               else [(5, true, fun exp ->
                  let np = p + 1 in
                  let c = Msg.Coord.shift coord s p np in
                  let next = {< p=np; coord=c; context=((self#skip np c) :> aux); del_ok = true; errors = (Errors.Deleted (s.[p], p, coord, exp))::errors >} in
                  if !Combinators.debug then printf "%s\n" (Errors.correct s next#errors);
                  next#getEOF k )]
            )))
      end      

    method loc = self#reloc coord

  end
    
let lget name regexp min k s = s#get name regexp min k
let llook        str     k s = s#look str k
let lregexp name str min k s = s#regexp name str min k
let leof                 k s = s#getEOF k