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

(** The type 

    {C [type ('stream, 'parsed, 'error) result = ('parsed * ('stream, 'parsed, 'error) parse * 'stream, 'error) Checked.t]}

    denotes a result of parsing a stream with a parse function. This result
    is either successful parse comprising parsed value of type ['parsed], the alternative
    parse function [('stream, 'parsed, 'error) parse], and the residual stream of type
    ['stream], or an error data of type ['error]
*)
type ('stream, 'parsed, 'error) result = ('parsed * ('stream, 'parsed, 'error) parse * 'stream, 'error) Checked.t

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

    {C [seb (fun x -> map (fun y -> (int_of_string y) + x) "1") (fun y -> map (fun z -> y + (int_of_string z)) "2")]}

    returns a function that takes an integer [x] and returns parse function [p] that eats successively
    ["1"] and ["2"], converts them into integers and sums with [x]. For example [(p 16) "1" "2"] returns [19].
    The name of combinator originates from "sequence with binding"
*)
val seb : ('d -> ('a, 'b, 'e) parse) -> ('b -> ('a, 'c, 'e) parse) -> ('d -> ('a, 'b * 'c, 'e) parse)

(** Infix synonym for [seq] *)
val (|>)  : ('a, 'b, 'd) parse -> ('a, 'c, 'd) parse -> ('a, 'b * 'c, 'd) parse

(** Infix synonym for [seb] *)
val (||>) : ('d -> ('a, 'b, 'e) parse) -> ('b -> ('a, 'c, 'e) parse) -> ('d -> ('a, 'b * 'c, 'e) parse)
 
(** Alternative combinator. [alt x y] returns parse function that eats that that either [x] or [y] eat *)
val alt : ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse

(** Infix synonym for [alt] *)
val (<|>) : ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse

(** Pruned alternative combinator. [alt x y] returns {U non-backtracking} parse function that eats 
    that that either [x] or [y] eat *)
val alp : ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse

(** Infix synonym for [alp] *)
val (<!>) : ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse

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
val (<*>) : ('a, 'b, 'c) parse -> ('a, 'b list, 'c) parse

(** Infix synonym for [iter] *)
val (<+>) : ('a, 'b, 'c) parse -> ('a, 'b list, 'c) parse

(** Guarded parse function constructor. [(guard predicate p) x] is [p]
    if [predicate x = true] and [fail] otherwise *)    
val guard : ('d -> bool) -> ('a, 'b, 'c) parse -> ('d -> ('a, 'b, 'c) parse)
