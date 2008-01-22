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
	String  of string     (** String terminal        *)
      | Term    of string     (** Non-string terminal    *)
      | Nonterm of string     (** Nonterminal            *)
      | Apply   of t * t list (** Rule application       *)
      | Star    of t          (** Iteration              *)
      | Plus    of t          (** Non-empty iteration    *)
      | Opt     of t          (** Optional expression    *)
      | Alt     of t list     (** Alteration             *)
      | Seq     of t list     (** Sequencing             *)
      | Group   of t          (** Expression in brackets *)
      | Custom  of string     (** Custom text            *)
    
  end

(** Rule definition representation *)
module Def :
  sig

    (** Main type *)
    type t 

    (** Constructor of simple definition. Takes name and body *)
    val make : string -> Expr.t -> t

    (** Constructor of parameterized definition. Takes name, 
        argument name and body 
    *)
    val makeP : string -> string -> Expr.t -> t

    (** TeX printer *)
    val toTeX : t -> string

  end
