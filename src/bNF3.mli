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

(** BNF tree implementation *)

(** BNF expression implementation *)
module Expr :
  sig

    (** Main type *)
    type t =
	String  of string        (** String terminal          *)
      | Term    of string        (** Non-string terminal      *)
      | Nonterm of string        (** Nonterminal              *)
      | Star    of t             (** Iteration                *)
      | Plus    of t             (** Non-empty iteration      *)
      | Opt     of t             (** Optional expression      *)
      | Alt     of t * t         (** Alteration               *)
      | Seq     of t * t         (** Sequencing               *)
      | Group   of t             (** Expression in brackets   *)
      | Apply   of t * string    (** Application to arguments *)
	    
  end

(** Rule definition representation *)
module Def :
  sig

    (** Main type: name of the rule, formal parameters (if any), rule body *)
    type t = string * string list * Expr.t

  end
