(*
 * Util: predefined Ostap utilities.
 * Copyright (C) 2006-2009
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

(** {1 Predefined parsing utilities} *)

(** {2 List parsing} *)

(** [listBy s delim item] parses a non-empty list of [item]s delimited by [delim] from a stream [s].
     Note that inside Ostap syntax extension the notation [listBy[delim][item]] should be used.
  *)
val listBy :   'a ->  ('a, 'b, < add : 'c -> 'c; .. > as 'c) Ostap.parse ->  ('a, 'd, 'c) Ostap.parse -> ('a, 'd list, 'c) Ostap.result

(** [list s item] parses a non-empty list delimited by commas. Inside Ostap syntax extensions this should
     be used in the form [list[item]].
 *)
val list :  (< look : string -> ('a, 'b, < add : 'c -> 'c; .. > as 'c) Ostap.result; .. > as 'a) ->
            ('a, 'd, 'c) Ostap.parse -> 
            ('a, 'd list, 'c) Ostap.result

(** [list0 s item] parses a possibly empty list delimited by commas. Inside Ostap syntax extensions this should
     be used in the form [list0[item]].
 *)
val list0 : (< look : string -> ('a, 'b, < add : 'c -> 'c; .. > as 'c) Ostap.result; .. > as 'a) ->
            ('a, 'd, 'c) Ostap.parse -> 
            ('a, 'd list, 'c) Ostap.result

(** {2 Expression parsing} *)

val expr :
  (('a -> ('b -> 'b) -> 'b -> 'b -> 'b) * ('c * 'a) list) array ->
  (< look : 'c -> ('d, 'e, < add : 'f -> 'f; .. > as 'f) Ostap.result; .. > as 'd, 'b, 'f) Ostap.parse -> 
  'd -> ('d, 'b, 'f) Ostap.result

(** Associativity functions; may be directly used in specification for [expr] function. *)
val left : ('a -> 'b -> 'c) -> ('d -> 'a) -> 'd -> 'b -> 'c
val right : ('a -> 'b -> 'c) -> ('c -> 'd) -> 'a -> 'b -> 'd

(** {2 Predefined lexer class} *)

class lexer :
  string ->
  object ('a)
    method col : int
    method coord : Msg.Coord.t
    method get :
      string -> Str.regexp -> ('a, Matcher.Token.t, Reason.t) Ostap.result
    method getCONST : ('a, Matcher.Token.t, Reason.t) Ostap.result
    method getEOF : ('a, Matcher.Token.t, Reason.t) Ostap.result
    method getIDENT : ('a, Matcher.Token.t, Reason.t) Ostap.result
    method line : int
    method loc : Msg.Locator.t
    method look : string -> ('a, Matcher.Token.t, Reason.t) Ostap.result
    method pos : int
    method prefix : int -> string
    method skip :
      int ->
      Msg.Coord.t -> [ `Failed of Msg.t | `Skipped of int * Msg.Coord.t ]
  end

(** {2 Miscellaneous} *)

(** [read fname] returns the content of the file [fname]. *)
val read : string -> string
