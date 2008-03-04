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

(** Reordering expression in according to priorities of its' operations *)

(** Signature to abstract expression being reordered *)
module type Expression =
  sig

    (** The type *)
    type t              
                 
    (** Priority of the operation *)
    val prio : t -> int               

    (** Whether the expression is protected (by brackets, prefix operators etc.) *) 
    val protected : t -> bool              

    (** Operands of tree node*)
    val sons : t -> t list         

    (** Constructor *) 
    val make : t -> t list -> t        
	  
  end

(** Functor to instantiate reorderer *)
module Make (E : Expression) :
  sig 

    (** Simple reordering function *)
    val sort : E.t -> E.t 

  end
