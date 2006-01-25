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

module type Item =
  sig

    type t

  end

module Make (X : Item) (Y : Item) =
  struct

    module H = Hashtbl.Make (
        struct 

	  type t = (X.t -> Y.t) * X.t

	  let hash                = Hashtbl.hash
	  let equal (f, x) (g, y) = (f == g) && (x = y)
	      
	end
       )

    type t = Y.t H.t
	  
    let t : t = H.create 1024

    let mem f x =
      try 
	H.find t (f, x)
      with
      | Not_found -> 
	  let y = f x in
	  H.add t (f, x) y;
	  y
	  
  end
