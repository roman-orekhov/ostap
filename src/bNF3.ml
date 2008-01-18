(*
 * BNF3: BNF tree representation.
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

module Expr =
  struct

    type t =
	String  of string
      | Term    of string
      | Nonterm of string
      | Star    of t
      | Plus    of t
      | Opt     of t
      | Alt     of t * t
      | Seq     of t * t
      | Group   of t
      | Apply   of t * string
	    
  end

module Def =
  struct

    type t = string * string list * Expr.t

  end
