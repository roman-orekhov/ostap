(*
 * Stream: lazy lists.
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

(** Implementation of lazy lists. *)

(** {2 General description} *)

(** Main type: lazy list of 'a-s *)
type 'a t

(** {2 Constructors} *)

(** [fromFunction f] constructs list of results of successive invocations of function [f]. Function
    [f] should raise exception [End_of_file] to indicate the end of list.
 *)
val fromFunction : (unit -> 'a) -> 'a t

(** [fromChannel f ch] works similar to [fromFuction] but applies [f] to [ch]; so [fromChannel input_string ch] 
    constructs a lazy list of strings from channel [ch].
 *)
val fromChannel : ('b -> 'a) -> 'b -> 'a t

(** Shortcut for [fromChanel input_char]. *)
val fromFile : in_channel -> char t

(** [fromIterator init succ] constructs a lazy list from some initial value [init] and successor function [succ].
    The list contains the values

      [fst (succ init);
      fst (succ (snd succ init)); 
      fst (succ (snd (succ (snd (succ init)))));
      ...]

    so [fromIterator 0 (fun i -> i, i+1)] constructs lazy list of natural numbers.
 *)
val fromIterator : 'b -> ('b -> 'a * 'b) -> 'a t

(** [fromGenerator init shift elem] constructs a lazy list from some initial value [init] and two functions [shift] and
    [elem]. The list contains the values

      [elem init;
      elem (shift init);
      elem (shift (shitf init));
      ...]
    
    So [fromGenerator 0 (fun i -> i+1) (fun i -> i)] also constructs a lazy list of natural numbers.
 *)     
val fromGenerator : 'b ->  ('b -> 'b) -> ('b -> 'a) -> 'a t

(** [fromList l] converts list [l] into lazy list. *)
val fromList : 'a list -> 'a t

(** [fromArray a] converts array [a] into lazy list. *)
val fromArray : 'a array -> 'a t

(** {2 Accessors} *)

(** [get s] reveals the head of the list which may be either [`EOS] (end of stream) or [`Cons (hd, tl)]. *)
val get : 'a t -> [ `EOS | `Cons of 'a * 'a t ]

(** [endOf s] returns [true] iff the list is empty. *)
val endOf : 'a t -> bool

(** [elem s] gets the next element of the list; raises [End_of_file] on empty list. *)
val elem : 'a t -> 'a

(** {2 Generic functions} *)

(** [map f s] maps function [f] to stream [s]. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** [iter f s] applies function [f] to each element of [s]. *)
val iter : ('a -> unit) -> 'a t -> unit

(** [fold f x [e0; ...; ek-1; ek]] calculates [f (...(f (f x ek) ek-1)... e1) e0]. *)
val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

(** [filter f s] filters out all elements of [s] on which [f] returns [false]. *)
val filter : ('a -> bool) -> 'a t -> 'a t

(** [zip s d] combines two streams [s] and [d] into the stream of pairs; if [s] and [d]
    have different lengths then the result stream have the length of shortest one.
  *)
val zip : 'a t -> 'b t -> ('a * 'b) t

(** [zip3 s d e] works similar to [zip] but returns the list of triplets. *)
val zip3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

(** [zip s d e f] workd similar to [zip] but returns the list of quadruples. *)
val zip4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
