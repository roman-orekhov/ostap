(*
 * Memo: memoization module.
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

(** A module to implement memoization of function applications *)

(** A signature to parameterize memoization functor *)
module type Item =
  sig

    (** Just a type *)
    type t

  end

(** Memoization functor: memoizes function applications *)
module Make (X : Item) (Y : Item) :
  sig

    (** [mem f x] performs a memoized application of a function [f] to an argument [x]:
        if the same application has been performed earlier then memoized value is returned
        without actual function application *)
    val mem : (X.t -> Y.t) -> X.t -> Y.t
	  
  end
