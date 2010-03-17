(*
 * Regexp: regular expression matching against streams.
 * Copyright (C) 2006-2010
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

(** Regular expression matching. *)

(** {2 General description} *)

(** Main type: ['a t] is regular expression over the alphabet ['a]. *)
type 'a t =
    Test  of string * ('a -> bool) (** [Test (n, p)] matches the single element 
                                       against the predicate [p];  argument [n] 
                                       is provided  for debugging/visualization 
                                       purposes. 
                                    *)
  | Aster of 'a t                  (** Regular zero-or-more repetition.      *)
  | Plus  of 'a t                  (** Regular one-or-more repetition.       *)
  | Opt   of 'a t                  (** Optional pattern.                     *)
  | Alter of 'a t list             (** Alternative.                          *)
  | Juxt  of 'a t list             (** Juxstaposition.                       *)
  | Arg   of string * 'a t         (** Argument capturing.                   *)                                       
  | BOS                            (** Begin of stream.                      *)
  | EOS                            (** End of stream.                        *)

(** Pretty-printer. *)
val toText : 'a t -> Pretty.printer 

(** String conversion. *)
val toString : 'a t -> string

(** {2 Regular expression diagram} *)
module Diagram :
  sig

    (** Type synonym for expression. *)
    type 'a expr = 'a t

    (** Type for the diagram. *)
    type 'a t 

    (** DOT visualizer. *)
    val toDOT : 'a t -> string

    (** Constructor; returns [`Duplicate name] when argument [name] is
        captured more then once.
     *)
    val make  : 'a expr -> [ `Duplicate of string | `Ok of 'a t ]

    (** Compiled diagram for nondeterministic matching. *)
    module Compiled :
      sig
  
        (** Type synonym for diagram. *)
        type 'a diagram = 'a t

        (** Type of the compiled diagram. *)
        type 'a t

        (** Constructor. *)
        val make : 'a diagram -> 'a t

        (** Matcher againt the stream. [matchStream t s] returns lazy list of 
            match results. Each match result is either [`Match ((s', args), tl)], where
            [s'] --- residual stream, [args name] --- the list of items matched by
            subexpression captured by argument [name] (empty list for non-captured names), 
            [tl] --- other match results, or [`End] if there are no (other) matches
            found. The matching is performed from the beginning of the stream.
          *)
        val matchStream : 'a t -> 'a Stream.t -> ([ `Match of (('a Stream.t * (string -> 'a list)) * 'b) | `End ] Lazy.t as 'b)
  
      end

  end
