(*
 * Pretty: basic set of pretty-printing combinators.
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

(** Pretty --- a generic pretty-printer combinator set *)

(** Printer type *)
type printer = Format.formatter -> unit

(** String conversion *)
val toString : printer -> string

(** Empty printer *)
val empty : printer

(** Newline printer *)
val newline : printer

(** Break printer (prints a logical break, see standard module Format) *)
val break : printer

(** Opens box (see standard module Format) *)
val box : printer

(** Opens vertical box (see standard module Format) *)
val vbox : printer

(** Opens horizontal box (see standard module Format) *)
val hbox : printer

(** Opens horizontal/vertical box (see standard module Format) *)
val hovbox : printer

(** Opens horizontal and vertical box (see standard module Format) *)
val hvbox : printer

(** Closes box (see standard module Format) *)
val endbox : printer

(** [string str] makes printer which prints [str] *)
val string : string -> printer

(** [int n] makes printer which prints [n] *)
val int : int -> printer

(** List printing combinator *)
val seq : printer list -> printer

(** Array printing combinator *)
val seqa : printer array -> printer

(** [listBy del list] prints elements of [list] delimited by [del] *)
val listBy : printer -> printer list -> printer

(** [arrayBy del array] prints elements of [array] delimited by [del] *)
val arrayBy : printer -> printer array -> printer

(** A synonym for [listBy (string "; ")] *)
val listBySemicolon : printer list -> printer 

(** A synonym for [listBy (string ", ")] *)
val listByComma : printer list -> printer

(** A synonym for [listBy (string " ")] *)
val listBySpace : printer list -> printer

(** A synonym for [listBy (seq [string "; "; break])] *)
val listBySemicolonBreak : printer list -> printer 

(** A synonym for [listBy (seq [string ", "; break])] *)
val listByCommaBreak : printer list -> printer 

(** A synonym for [listBy (seq [string " "; break])] *)
val listBySpaceBreak : printer list -> printer 

(** A synonym for [listBy break] *)
val listByBreak : printer list -> printer 

(** A synonym for [arrayBy (string "; ")] *)
val arrayBySemicolon : printer array -> printer 

(** A synonym for [arrayBy (string ", ")] *)
val arrayByComma : printer array -> printer

(** A synonym for [arrayBy (string " ")] *)
val arrayBySpace : printer array -> printer

(** A synonym for [arrayBy (seq [string "; "; break])] *)
val arrayBySemicolonBreak : printer array -> printer 

(** A synonym for [arrayBy (seq [string ", "; break])] *)
val arrayByCommaBreak : printer array -> printer 

(** A synonym for [arrayBy (seq [string " "; break])] *)
val arrayBySpaceBreak : printer array -> printer 

(** A synonym for [arrayBy break] *)
val arrayByBreak : printer array -> printer 

(** {2 Shortcuts for boxed formatting} *)

(** Prints its argument within box (see standard module Format) *)
val boxed : printer -> printer

(** Prints its argument within hbox (see standard module Format) *)
val hboxed : printer -> printer

(** Prints its argument within vbox (see standard module Format) *)
val vboxed : printer -> printer

(** Prints its argument within hovbox (see standard module Format) *)
val hovboxed : printer -> printer

(** Prints its argument within hvbox (see standard module Format) *)
val hvboxed : printer -> printer

(** [brboxed left right p] surrounds [p] by "brackets" [left] and [right] *)
val brboxed : printer -> printer -> printer -> printer

(** [prboxed prefix p] prefixes [p] by [prefix] *)
val prboxed : printer -> printer -> printer

(** Synonym for [brboxed (string "(") (string ")")] *)
val rboxed : printer -> printer

(** Synonym for [brboxed (string "[") (string "]")] *)
val sboxed : printer -> printer

(** Synonym for [brboxed (string "{") (string "}")] *)
val cboxed : printer -> printer


