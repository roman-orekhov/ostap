(*
 * Msg: parsing message module.
 * Copyright (C) 2006-2008
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

(** Parsing message interface *)

(** {2 Messaging operations} *)

(** Text coordinate *)
module Coord :
  sig

    (** Type synonym: line, column *)  
    type t = int * int

    (** String conversion *)
    val toString : t -> string

  end

(** Various ways to denote the location in the source text *)
module Locator :
  sig

    (** Locator type *)
    type t =
        No                             (** No location defined             *)
      | Point    of Coord.t            (** One point in the text           *)
      | Interval of Coord.t * Coord.t  (** Contiguous interval of points   *)
      | Set      of t list             (** Non-contiguous set of locations *)

    (** String conversion *)
    val toString : t -> string

  end

(** Type of the message; [phrase] stands for format string, [args] --- for actual
    parameters, [loc] --- locator. Format string may contain parameter referencies of the
    form ["%0"], ["%1"], ["%2"] etc. These referencies to be substituted with corresponding actual
    values (e.f. [args.(0)], [args.(1)] etc.) during toString vizualization. For example,
    [toString {phrase="%0 %1 not found"; args=[|"type"; "int"|]; loc=No}] is
    ["type int not found"]
*)
type t = {phrase: string; args: string array; loc: Locator.t} 

(** General constructor *) 
val make : string -> string array -> Locator.t -> t

(** No parameters, no locator *)
val phrase : string -> t

(** No locators *)
val orphan : string -> string array -> t

(** Substitute parameters *)
val string : t -> string

(** Visualization with parameter substitution *)
val toString : t -> string

(** Augment the message with the location (replaces [Locator.No] with the [loc]) *)
val augment : t -> Locator.t -> t

(** Augment the list of messages with the location *)
val augmentList : t list -> Locator.t -> t list

(** Extend the message by some detailed information *)
val extend : t -> string -> t

(** Extend the list of messages by some detailed information *)
val extendList : t list -> string -> t list

