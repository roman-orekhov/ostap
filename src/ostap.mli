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

(** Ostap --- a common set of parser combinators. *)

(** The name of this library originates from Ostap Bender --- the central character
    of Ilya Ilf and Eugene Petrov's comedy "The Twelve Chairs". Bender is
    generally referred to as "The Great Combinator" since the word
    "combinator" in Russian also means "a swindler", "a sly man" etc.
*)

(** {2 Main parsing types } *)

(** Type pattern for result of parsing. Result is either a parsed value, an error with a list of 
    error details or a failure with a list of details. The difference between error and failure is 
    that error indicates the real error that has to be reported while failure only means that 
    taken way to parse the source was unsuccessful (but some other may be ok). For example,
    parsing the stream "B" with rule "A", "B" results in failure since no items was consumed from
    the stream; on the other hand parsing "AC" with the same rule returns error since "A" was
    succesfully matched against the stream, but "B" then failed.
*)
type ('a, 'b) tag = Parsed of 'a | Error of 'b list | Failed of 'b list

(** The type 

    {C [type ('stream, 'parsed, 'error) result = ('parsed * 'stream, 'error) tag]}

    denotes a result of parsing a stream with a parse function. This result
    is either successful parse comprising parsed value of type ['parsed] and the residual 
    stream of type ['stream], or an error/failure data of type ['error].
*)
type ('stream, 'parsed, 'error) result = ('parsed * 'stream, 'error) tag

(** The type 

    {C [type ('stream, 'parsed, 'error) parse  = 'stream -> ('stream, 'parsed, 'error) result]}

    corresponds to a parse function. Parse function takes a stream of type ['stream] and
    returns parsing result.
*)
type ('stream, 'parsed, 'error) parse  = 'stream -> ('stream, 'parsed, 'error) result

(** {2 General parse functions } *)

(** [rise s] returns [Parsed (s, s)] and so "rises" the stream [s] as a successful
    parse result.
*)
val rise : ('a, 'a, 'b) parse

(** {2 General parsing combinators} *)

(** [map f p] applies [f] to the result of [p], if [p] succeeded, or fails otherwise. *)
val map : ('b -> 'c) -> ('a, 'b, 'd) parse -> ('a, 'c, 'd) parse 

(** Infix synonim for [map]. Note: the order of parameters is inverted. *)
val (-->) : ('a, 'b, 'd) parse -> ('b -> 'c) -> ('a, 'c, 'd) parse

(** Sequence combinator. [seq x y] is constructs a parser function to parse successively by [x] and [y]. 
    Parsed by [x] value is passed to [y] as a context. 
*)
val seq : ('a, 'b, 'e) parse -> ('b -> ('a, 'c, 'e) parse) -> ('a, 'c, 'e) parse

(** Infix synonym for [seq]. *)
val (|>)  : ('a, 'b, 'e) parse -> ('b -> ('a, 'c, 'e) parse) -> ('a, 'c, 'e) parse
 
(** Alternative combinator. [alt x y] returns parse function that eats that that either [x] or [y] eat.
    [alt x y] tries [y] even if [x] returned [Error].
*)
val alt : ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse

(** Infix synonym for [alt]. *)
val (<|>) : ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse

(** Pruned alternative combinator. [alc x y] returns parse function that eats 
    that that either [x] or [y] eat; if [x] returned [Error] then [y] is not tried.
*)
val alc : ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse

(** <!> is infix synonym for [alc]. *)
val (<!>) : ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse

(** Optional combinator. [opt x] returns parse function that eats either [x] or nothing. *)
val opt : ('a, 'b, 'c) parse -> ('a, 'b option, 'c) parse

(** <?> is infix synonym for [opt]. *)
val (<?>) : ('a, 'b, 'c) parse -> ('a, 'b option, 'c) parse

(** Zero-or-more iteration. [many x] returns parse function that {b eagerly} eats zero of more
    sucessive occurencies of items eaten by [x]. 
*)
val many : ('a, 'b, 'c) parse -> ('a, 'b list, 'c) parse

(** One-or-more iteration. [some x] returns parse function that {b eagerly} eats one of more
    sucessive occurencies of items eaten by [x]. 
*)
val some : ('a, 'b, 'c) parse -> ('a, 'b list, 'c) parse

(** Infix synonym for [many]. *)
val (<*>) : ('a, 'b, 'c) parse -> ('a, 'b list, 'c) parse

(** Infix synonym for [some]. *)
val (<+>) : ('a, 'b, 'c) parse -> ('a, 'b list, 'c) parse

(** Guarded parse function constructor. [guard p predicate] 
    checks [predicate] against successfull parsed by [p] value and 
    turns it into [Failed []] if this check failed.
*)    
val guard : ('a, 'b, 'c) parse -> ('b -> bool) -> ('a, 'b, 'c) parse
