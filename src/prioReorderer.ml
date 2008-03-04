(*
 * PrioReorderer: reordering expression trees by priorities.
 * Copyright (C) 2008
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

module type Expression =
  sig

    type t              
                 
    val prio      : t -> int               
    val protected : t -> bool
    val sons      : t -> t list
    val make      : t -> t list -> t
	  
  end
      
module Make (E : Expression) = 
  struct
    
    open E
      
    let map f t = make t (List.map f (sons t))

    let rec sort expr =
      let rec reduce p ((oper, opnd) as stacks) = 
        match oper with
        | (t, p') :: oper' when (p < p') ->
            let r::l::tl = opnd in
            reduce p (oper', (make t [l; r]) :: tl)
        | _ -> stacks
      in
      let rec putin t ((oper, opnd) as stacks) =
	let sons = sons t in
	if protected t 
	then (oper, (map sort t) :: opnd)
	else
          match sons with
          | [l; r] -> 
              let p = prio t in
              let oper', opnd' = reduce p (putin l stacks) in
              putin r ((t, p) :: oper', opnd')
	  | _ -> (oper, (map sort t) :: opnd)
      in 
      let _, result::_ = reduce (-1) (putin expr ([], [])) in
      result    

  end
    























