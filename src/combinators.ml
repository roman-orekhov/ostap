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
open Lazy

let debug = ref false
let cnt2 = ref 0

type ('stream, 'token, 'result) cont  = 'token -> 'stream -> 'result steps
and  ('stream, 'token, 'result) parse = ('stream, 'token, 'result) cont -> 'stream -> 'result steps
and  ('stream, 'token, 'result) parsed = ('stream, 'token, 'result) cont -> 'result steps
and  'a step =
   | Step  of int
   (* bool - input consumption indicator *)
   | Fail  of (strings * (int * bool * (strings -> 'a steps)) list) Lazy.t
   | End_alt of int
   | Result of 'a
and  strings = string list
and  'a steps = 'a step Ostream.t

let rec best a b =
   let sa, resa = Ostream.get a
   and sb, resb = Ostream.get b in
   match sa, sb with
   | End_alt n, _         -> best resa b
   | _        , End_alt n -> best a resb
   | Fail l   , Fail r    -> Ostream.one (Fail (lazy (let sl, fl = force l and sr, fr = force r in (sl@sr, fl@fr))))
   | Fail _   , _
   | _        , Result _  -> b
   | _        , Fail _
   | Result _ , _         -> a
   | Step n   , Step m    ->
      if n = m then      Ostream.consL (Step n) (lazy (best resa resb)                              )
      else if n < m then Ostream.consL (Step n) (lazy (best resa (Ostream.cons (Step (m - n)) resb)))
      else               Ostream.consL (Step m) (lazy (best (Ostream.cons (Step (n - m)) resa) resb))

let lookahead = ref 10

let rec eval s =
   let step, rest = Ostream.get s in
   match step with
   | Step n     -> eval rest
   | Fail f     -> let (ss, fs) = force f in eval (getCheapest !lookahead ss fs)
   | End_alt n  -> eval rest
   | Result res -> res
and getCheapest n ss = function
   | [] -> failwith "no correcting alternative found"
   | [(_, _, f)] -> f ss
   | l  ->
      snd (
         List.fold_right 
            (fun (cost, progresses, f) ((best_cost, _) as acc) ->
               if cost < best_cost
               then
                  let path = f ss in
                  let min_cost_using_lookahead, p = traverse progresses path cost (best_cost, path) n in
                  if min_cost_using_lookahead < best_cost then (min_cost_using_lookahead, if progresses then path else p) else acc
               else acc 
            )
            l (max_int, Ostream.fromList [])
          )
(* MAYBE RAISE FAIL PENALTY AND LOWER STEP PENALTY WITH N GOING DOWN? FFFSSS SHOULD BE BETTER THAN SSSFFF *)
and traverse progressed s v (c, res) = function
   | 0 -> v, res
   | n -> let step, rest = Ostream.get s in
      match step with
      | Result _    -> v (* min_int *), res
      | Step _      -> traverse true rest (v - 0) (c, res) (n - 1)
      (* alternatively one can save endas and instead of returning (f ss) return Fail (lazy (ss, [f])) with correct f *)
      | End_alt cnt -> traverse progressed rest (v - 0) (c, res) (n - 0)
      | Fail f      -> 
         let (ss, fs) = force f in
         List.fold_right 
            (fun (cost, progresses, f) ((c, _) as acc) ->
               if v + cost < c
               then (
                  let path = f ss in
                  let v, p = traverse (progressed || progresses) path (v + cost) (c, if progressed then res else (* (printf "res changed!\n"; *)path) (n - 1) in
                  if v < c then v, p else acc)
               else acc
            )
            fs (c, res)

let return x k s = k x s
let empty    k s = return () k s
let fail_stream r = Ostream.one (Fail (lazy_from_val ((match r with None -> [] | Some r -> [r]), [])))
let fail   r k s = fail_stream r
let lift     k s = return s k s
let sink   p k s = p (fun p_result s -> k p_result p_result) s

let map  f p k s = p (fun a s -> k (f a) s) s
let (-->) p f = map f p

(* let seq = p (fun p_result s -> q (fun q_result s -> k (p_result q_result) s) s) s *)
let seq  p q k s = p (fun p_result s -> q p_result k s) s
let (|>)  = seq

let both p q k s = best (p k s) (q k s)

(*
let rec removeEnd s =
   let y, rest = Ostream.get s in
   match y with
   | End_alt -> removeEnd rest
   | _ -> s
*)
let rec best_prio end_id a b =
   let sa, resa = Ostream.get a
   and sb, resb = Ostream.get b in
   (* sb = End_alt when b is a continuation, in e.g. ((a|b)|) -> a_enda_enda|b_enda *)
   match sa, sb with
   | Result _ , _        -> a
   | Step _   , End_alt n-> Ostream.consL sb (lazy (best_prio end_id a resb))
   | Fail f   , End_alt n-> Ostream.consL sb (lazy (best_prio end_id a resb))
   | Fail l   , Fail r   -> Ostream.one (Fail (lazy (let sl, fl = force l and sr, fr = force r in (sl@sr, fl@fr))))
   | End_alt n, _        -> if end_id = n then resa else Ostream.consL sa (lazy (best_prio end_id resa b))
   | _        , Fail f   -> a
   | Fail f   , _        -> b
(*   SHOULD THE PARSER HAVE NO EOF - USED TO HANG HERE, SEE TEST013 AND SWIERSTRA *)
   | _        , Result _ -> Ostream.consL sa (lazy (best_prio end_id resa b))
   | Step n   , Step m   ->
      if n = m then      Ostream.consL (Step n) (lazy (best_prio end_id resa resb)                              )
      else if n < m then Ostream.consL (Step n) (lazy (best_prio end_id resa (Ostream.cons (Step (m - n)) resb)))
      else               Ostream.consL (Step m) (lazy (best_prio end_id (Ostream.cons (Step (n - m)) resa) resb))
(*
((bc|)bc|b)
(bc_e | _e)bc_e | b_e
Step 1, c_e | enda fail k
enda (Step 1, c_e) k
*)
(* (* overflows in caml internal lazy *)
let rec removeEnd s =
   let y, rest = Ostream.get s in
   match y with
   | Step n   -> if !debug then printf "rem: step %d\n" n; Ostream.consL y (lazy (removeEnd rest))
   | End_alt  -> if !debug then printf "rem: enda\n"; rest
   | Fail f   -> if !debug then printf "rem: fail\n"; Ostream.one (Fail (lazy (let sl, fl = force f in sl, List.map (fun f -> fun exp -> let (cost, path) = f exp in (cost, removeEnd path)) fl)))
   | Result _ -> s
*)
let prio p q k s =
   let c, l =
      cnt2 := !cnt2 + 1;
      let c = !cnt2 in
      let k' a s = Ostream.consL (End_alt c) (lazy (k a s)) in
      c, p k' s
   in
   (*removeEnd*) (best_prio c l (q k s))
let (<|>) = both
let (<-|>)= prio
let alt = prio

let opt_default v p k s = (p <-|> return v) k s
let opt    p k s = opt_default None (p --> (fun r -> Some r)) k s
let (<?>) = opt

let manyFold f init p =
   let rec pfm acc k s = opt_default acc (p |> (fun p_result -> pfm (f acc p_result))) k s in
   pfm init

let many p = (manyFold (fun acc x -> fun l -> acc (x::l)) (fun x -> x) p) --> (fun t -> t [])

let (<*>) = many

let someFold f init p = p |> (fun h -> manyFold f (f init h) p)

let some p = (someFold (fun acc x -> fun l -> acc (x::l)) (fun x -> x) p) --> (fun t -> t [])

let (<+>) = some

let lastSteps = Ostream.fromGenerator 0 (fun i -> i+1) (fun i -> if i <= !lookahead then Step 0 else raise End_of_file)

let finish p s = p (fun p_result rest -> Ostream.concat lastSteps (Ostream.one (Result (p_result, rest)))) s

let guard p f r k s =
   p (fun p_result s ->
      if f p_result
      then k p_result s
      else Ostream.one (Fail (lazy ((match r with None -> [] | Some r -> [r p_result]), [])))
      (* (5, false, fun exp -> k p_result s) *)
   ) s
(*
   let rec eval acc s =
      let step, rest = Ostream.get s in
      match step with
      | Step n -> eval (if n>0 then (Step n)::acc else acc) rest
      | Fail f -> let (ss, fs) = force f in eval acc (getCheapest 5 (List.map (fun f -> f ss) fs))
      | Result res -> acc, res in
   let stps, (p_result, rest) = eval [] (finish p s) in
   if f p_result
   then Ostream.concatL (Ostream.fromList (List.rev stps)) (lazy (k p_result rest))
   else Ostream.one (Fail (lazy ((match r with None -> [] | Some r -> [r p_result]), [fun exp -> (5, k p_result rest)])))
*)
let comment p str k s =
   let replace s =
      let step, rest = Ostream.get s in
      match step with
      | Fail f -> Ostream.one (Fail (lazy (let _, fs = force f in [str], fs)))
      | _ -> s in
   replace (p k s)
(*
   p (fun p_result s -> replace (k p_result s)) s
*)

let altl = function
   | []   -> fail (Some "no alts in <-|>")
   | [h]  -> h
   | h::t -> List.fold_left (<-|>) h t

let prohibit p k s =
   let rec inner s' =
      let step, rest = Ostream.get s' in
      match step with
      | Step n     -> inner rest
      | Fail f     -> k () s
      | End_alt n  -> inner rest
      | Result res -> invalid_arg "prohibit" in
   try
      inner (p (fun p_result rest -> lastSteps) s)
   with End_of_file -> fail_stream (Some "prohibited")

let parse p s = eval (finish p s)
