(*
 * Ostap: basic set of parser combinators.
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

(** {1 Parsing combinators with backtracking} *)

(** The name of this library originates from Ostap Bender --- the central character
    of the Ilya Ilf and Eugene Petrov's comedy "The Twelve Chairs". Bender is
    generally referred to as "The Great Combinator" since the word
    "combinator" is Russian also means "a swindler", "a sly man" etc.
*)

(** {1 Main parsing types } *)

(** Type pattern for result of parsing. Result is either parsed value, an error with a list of 
    error details or a failure with a list of details *)
type ('a, 'b) tag = Parsed of 'a | Error of 'b list | Failed of 'b list

(** The type 

    {C [type ('stream, 'parsed, 'error) result = ('parsed * ('stream, 'parsed, 'error) parse * 'stream, 'error) tag]}

    denotes a result of parsing a stream with a parse function. This result
    is either successful parse comprising parsed value of type ['parsed], the alternative
    parse function [('stream, 'parsed, 'error) parse], and the residual stream of type
    ['stream], or an error data of type ['error]
*)
type ('stream, 'parsed, 'error) result = ('parsed * ('stream, 'parsed, 'error) parse * 'stream, 'error) tag

(** The type 

    {C [type ('stream, 'parsed, 'error) parse  = 'stream -> ('stream, 'parsed, 'error) result]}

    denotes a parse function. Parsing function takes a stream of type ['stream] and
    returns parsing result
*)
and  ('stream, 'parsed, 'error) parse  = 'stream -> ('stream, 'parsed, 'error) result

(** {1 General parse functions } *)

(** [fail] fails on each stream. *)
val fail : ('a, 'b, 'c) parse

(** [map f p] applies [f] to the succesfull result of [p] or fails. For example, 
    [map int_of_string "1"] is a function that tries to eat ["1"] out of input stream, and
    returns integer [1] on success.
*)
val map : ('b -> 'c) -> ('a, 'b, 'd) parse -> ('a, 'c, 'd) parse 

(** {1 General parsing combinators} *)

(** [seq x y] returns parse function for sequence of parse first by [x], then 
    by [y]. For example [seq "John" " Doe"] is a parser function to eat "John Doe" out of
    input steram 
*)
val seq : ('a, 'b, 'd) parse -> ('a, 'c, 'd) parse -> ('a, 'b * 'c, 'd) parse

(** [seb x y] is similar to [seq] but allows for passing the eaten by [x] value to [y].
    For example 

    {C [seb "1" (fun y -> map (fun z -> y + (int_of_string z)) "2")]}

    returns a parse function [p] that eats successively ["1"] and ["2"], converts them into 
    integers and sums them. The name of combinator originates from "sequence with binding"
*)
val seb : ('a, 'b, 'e) parse -> ('b -> ('a, 'c, 'e) parse) -> ('a, 'b * 'c, 'e) parse

(** Infix synonym for [seq] *)
val (|!>)  : ('a, 'b, 'd) parse -> ('a, 'c, 'd) parse -> ('a, 'b * 'c, 'd) parse

(** Infix synonym for [seb] *)
val (||!>) : ('a, 'b, 'e) parse -> ('b -> ('a, 'c, 'e) parse) -> ('a, 'b * 'c, 'e) parse

(** [sec x y] operates similar to [seq] but avoids backtracking *)
val sec : ('a, 'b, 'd) parse -> ('a, 'c, 'd) parse -> ('a, 'b * 'c, 'd) parse

(** [secb x y] operates similar to [seb] but avoids backtracking *)
val secb : ('a, 'b, 'e) parse -> ('b -> ('a, 'c, 'e) parse) -> ('a, 'b * 'c, 'e) parse

(** Infix synonym for [sec] *)
val (|>)  : ('a, 'b, 'd) parse -> ('a, 'c, 'd) parse -> ('a, 'b * 'c, 'd) parse

(** Infix synonym for [secb] *)
val (||>) : ('a, 'b, 'e) parse -> ('b -> ('a, 'c, 'e) parse) -> ('a, 'b * 'c, 'e) parse

(** [cut x] cuts the backtracking of parser function [x] *)
val cut : ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse
 
(** Alternative combinator. [alt x y] returns parse function that eats that that either [x] or [y] eat *)
val alt : ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse

(** Infix synonym for [alt] *)
val (<!>) : ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse

(** Pruned alternative combinator. [alc x y] returns {b non-backtracking} parse function that eats 
    that that either [x] or [y] eat *)
val alc : ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse

(** Infix synonym for [alp] *)
val (<|>) : ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse

(** Optional combinator. [opt x] returns parse function that eats either [x] or nothing *)
val opt : ('a, 'b, 'c) parse -> ('a, 'b option, 'c) parse

(** Infix synonym for [opt] *)
val (<?>) : ('a, 'b, 'c) parse -> ('a, 'b option, 'c) parse

(** Zero-or-more iteration. [iterz x] returns parse function that eats zero of more
    sucessive occurencies of items eaten by [x] 
*)
val iterz : ('a, 'b, 'c) parse -> ('a, 'b list, 'c) parse

(** One-or-more iteration. [iter x] returns parse function that eats one of more
    sucessive occurencies of items eaten by [x] 
*)
val iter : ('a, 'b, 'c) parse -> ('a, 'b list, 'c) parse

(** Infix synonym for [iterz] *)
val (<!*>) : ('a, 'b, 'c) parse -> ('a, 'b list, 'c) parse

(** Infix synonym for [iter] *)
val (<!+>) : ('a, 'b, 'c) parse -> ('a, 'b list, 'c) parse

(** Zero-or-more iteration with no backtracking. [iterzc x] returns parse function that 
    eats zero of more sucessive occurencies of items eaten by [x] 
*)
val iterzc : ('a, 'b, 'c) parse -> ('a, 'b list, 'c) parse

(** One-or-more iteration with no backtracking. [iterc x] returns parse function that 
    eats one of more sucessive occurencies of items eaten by [x] 
*)
val iterc : ('a, 'b, 'c) parse -> ('a, 'b list, 'c) parse

(** Infix synonym for [iterzc] *)
val (<*>) : ('a, 'b, 'c) parse -> ('a, 'b list, 'c) parse

(** Infix synonym for [iterc] *)
val (<+>) : ('a, 'b, 'c) parse -> ('a, 'b list, 'c) parse

(** Guarded parse function constructor. [guard p predicate] is 
    checks [predicate] against successfull parsed by [p] value and 
    turns it into [Failed []] if this check failed.
*)    
val guard : ('a, 'b, 'c) parse -> ('b -> bool) -> ('a, 'b, 'c) parse
