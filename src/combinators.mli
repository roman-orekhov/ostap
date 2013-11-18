(*
 * Ostap: a common set of parser combinators.
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

(** Ostap --- a general set of parser combinators. *)

(** The name of this library originates from {{:http://en.wikipedia.org/wiki/Ostap_Bender} Ostap Bender}
    --- the central character of Ilya Ilf and Eugene Petrov comedy "The Twelve Chairs". Bender is
    generally referred to as "The Great Combinator" since the word "combinator" in Russian also means 
    "a swindler", "a sly man" etc.
 *)

type ('stream, 'token, 'result) cont  = 'token -> 'stream -> 'result steps
and  ('stream, 'token, 'result) parse = ('stream, 'token, 'result) cont -> 'stream -> 'result steps
and  ('stream, 'token, 'result) parsed = ('stream, 'token, 'result) cont -> 'result steps
and  'a step =
   | Step  of int
   | Fail  of (strings * (int * bool * (strings -> 'a steps)) list) Lazy.t
   | End_alt of int
   | Result of 'a
and  strings = string list
and  'a steps = 'a step Ostream.t

val return   : 'token -> ('stream, 'token, 'result) parse
val empty    : ('stream, unit, 'result) parse
val fail     : string option -> ('stream, 'token, 'result) parse
val lift     : ('stream, 'stream, 'result) parse
val sink     : ('stream, 'stream, 'result) parse -> ('stream, 'stream, 'result) parse
val map      : ('a -> 'b) -> ('stream, 'a, 'result) parse -> ('stream, 'b, 'result) parse
val (-->)    : ('stream, 'a, 'result) parse -> ('a -> 'b) -> ('stream, 'b, 'result) parse
(*
val seq      : ('stream, 'a -> 'b, 'result) parse -> ('stream, 'a, 'result) parse -> ('stream, 'b, 'result) parse
val (|>)     : ('stream, 'a -> 'b, 'result) parse -> ('stream, 'a, 'result) parse -> ('stream, 'b, 'result) parse
*)
val seq      : ('stream, 'a, 'result) parse -> ('a -> ('stream, 'b, 'result) parse) -> ('stream, 'b, 'result) parse
val (|>)     : ('stream, 'a, 'result) parse -> ('a -> ('stream, 'b, 'result) parse) -> ('stream, 'b, 'result) parse
val alt      : ('stream, 'token, 'result) parse -> ('stream, 'token, 'result) parse -> ('stream, 'token, 'result) parse
val both     : ('stream, 'token, 'result) parse -> ('stream, 'token, 'result) parse -> ('stream, 'token, 'result) parse
val (<|>)    : ('stream, 'token, 'result) parse -> ('stream, 'token, 'result) parse -> ('stream, 'token, 'result) parse
val (<-|>)   : ('stream, 'token, 'result) parse -> ('stream, 'token, 'result) parse -> ('stream, 'token, 'result) parse
val prio     : ('stream, 'token, 'result) parse -> ('stream, 'token, 'result) parse -> ('stream, 'token, 'result) parse
val opt      : ('stream, 'token, 'result) parse -> ('stream, 'token option, 'result) parse
val (<?>)    : ('stream, 'token, 'result) parse -> ('stream, 'token option, 'result) parse
val manyFold : ('a -> 'b -> 'a) -> 'a -> ('stream, 'b, 'result) parse -> ('stream, 'a, 'result) parse
val many     : ('stream, 'a, 'result) parse -> ('stream, 'a list, 'result) parse
val (<*>)    : ('stream, 'a, 'result) parse -> ('stream, 'a list, 'result) parse
val someFold : ('a -> 'b -> 'a) -> 'a -> ('stream, 'b, 'result) parse -> ('stream, 'a, 'result) parse
val some     : ('stream, 'a, 'result) parse -> ('stream, 'a list, 'result) parse
val (<+>)    : ('stream, 'a, 'result) parse -> ('stream, 'a list, 'result) parse
val guard    : ('stream, 'token, 'result) parse -> ('token -> bool) -> ('token -> string) option -> ('stream, 'token, 'result) parse
val comment  : ('stream, 'token, 'result) parse -> string -> ('stream, 'token, 'result) parse
val altl     : ('stream, 'token, 'result) parse list -> ('stream, 'token, 'result) parse
val parse    : ('stream, 'token, 'token * 'stream) parse -> 'stream -> 'token * 'stream
val debug    : bool ref
val lookahead: int ref
