(*
 * Matcher: simple lexer pattern.
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

(** Implementation of streams as objects. *)

(** {2 Simple implementation of streams as objects} *)

(** Token: a string augmented with text coordinates. *)
module Token :
  sig

    (** Type of the token. *)
    type t = string * Msg.Coord.t

    (** String visualizer. *)
    val toString : t -> string

    (** Text coordinate. *)
    val loc  : t -> Msg.Locator.t

    (** String image. *)
    val repr : t -> string

  end

(** [shiftPos loc s b e] takes text coordinates [loc], string [s] and two indexes [b] and [e], 
    scans [s] from [b] to [e] inclusively and shifts [loc] to take into account newlines.
*)
val shiftPos : Msg.Coord.t -> string -> int -> int -> Msg.Coord.t

(** Matcher pattern to inherit from to obtain the steream implementation. 
    [matcher make s pos loc] creates an object that helps to match regular
    expressions against string [s] starting from position [pos]. [loc] is the text coordinate of the 
    position [pos], [make] is a constructor to create residual parser (usually just a constructor
    of inherited class). *)
class virtual ['a] matcher : (string -> int -> Msg.Coord.t -> 'a) -> string -> int -> Msg.Coord.t ->
  object 

    (** [get name expr] is a parse function that parses regular expression [expr] at the current
        position. [name] is just a name for diagnostic purposes. 
    *)
    method get : string -> Str.regexp -> ('a, Token.t, Msg.t) Ostap.result

    (** [getEOF] detects end of stream. *)
    method getEOF : ('a, Token.t, Msg.t) Ostap.result

    (** [getFIRST] gets an empty token at the current position and serves to obtain
        coordinate of first symol of constructon. 
    *)
    method getFIRST : ('a, Token.t, Msg.t) Ostap.result

    (** [getLAST] gets an empty token at the  current position and serves to obtain
        coordinate of last symol of constructon. 
    *)
    method getLAST : ('a, Token.t, Msg.t) Ostap.result

    (** [look str] looks at the current stream for string [str]. *)
    method look : string -> ('a, Token.t, Msg.t) Ostap.result

    (** Virtual method to skip meaningless symbols (e.g. whitespaces); returns
        position and coordinates of first meaningful symbol. [skip] is implicitly
        called prior to all of the above methods except for the [getLAST].
    *)
    method virtual skip : int * Msg.Coord.t

  end
