(*
 * Matcher: simple lexer pattern.
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

(** Implementation of streams as objects *)

(** {2 General description} *)

(** This module provides a sample pattern for implementation of streams as objects.
    A stream is represented by a number of methods each of which tries to
    consume a certain lexeme from the stream. Thought Ostap is generally
    invariant with regard to stream implementation this approach helps to abstract
    parsers from lexers. The main use case for the implementation is to
    provide lexers for parser functions created via [Pa_ostap] syntax extensions,
    but you may use it on your own.
 
    The pattern given below is in turn just a sample --- it implements some
    simplest way to produce lexers using [Str] standard library; additionally
    it predefines types for error signaling and token representation. You must
    not generally use these types since you may implement objective lexer
    in any way you want. For example, you may write a grammar expression, say

    [let p = rule IDENT "(" IDENT (-"," IDENT)* ")"]
 
    inspect its type 

    [(< getIDENT : ('a, 'b, 'c) Ostap.result;
         look : string -> ('a, 'd, 'c) Ostap.result; .. >
       as 'a) ->
      ('a, 'b * 'd * 'b * 'b list * 'd, 'c) Ostap.result
    ]

    and conclude that you need a stream of type

    [< getIDENT : ('a, 'b, 'c) Ostap.result;
        look : string -> ('a, 'd, 'c) Ostap.result; .. >
      as 'a
    ]

    Here ['a] is the type of stream itself, ['b] --- the type of data, returned by successful
    application of [getIDENT], ['c] --- the type of data returned at error/failure, ['d] ---
    the type of data, returned by succesful application of [look]. [look] is just a member
    function to match a string against the stream.

    Using [matcher] you may easily provide an appropriate stream implementation by
    inheriting class [matcher]. The detailed description is given
    in the following section; for now we provide a simple example for the last
    grammar expression:


    {v open String v}
    {v open Str v}
    {v open Ostap v}
    {v open Matcher v}

    {v let ws      = regexp "[ \n\t\r]+" v}   
    {v let ident   = regexp "[a-zA-Z_]\([a-zA-Z_0-9]\)*"] v}
	
    {v class lexer s p coord = v} 
    {v     object (self) v}

    {v        inherit matcher s p coord v}
   
    {v        method skip p coord = v}
    {v 	         if string_match ws s p v} 
    {v 	         then v}
    {v 	            let m = matched_string s in v}
    {v 	            (p+length m), (shiftPos coord m 0 (length m)) v}
    {v 	         else p, coord v}
     
    {v        method getIDENT = self#get "identifier" ident v}
         
    {v     end v}

    {v let ofString s = new lexer s 0 (1, 1) v}
       
    Lexer is an immutable object sprang over the triple of attributes: the
    string [s] to parse, the position [p] within this string and two-dimensional
    coordinates [coord] of predefined type [Coord.t]. Each time the lexem is consumed
    from the stream the position and coordinates are shifted and a fresh instance
    of lexer is returned as the residual stream. The constructor of base class [matcher] 
    takes the string, position and coordinates as its arguments. 

    To turn [matcher] into true lexer we have to define the following methods:

    {ol
       {li method [skip] to skip meaningless symbols (whitespaces, comments etc.) from 
        the stream; you have to supply its implementation since it is declared  
        abstract in the base class.}
       {li method [getL] for any lexem [L] to recognize. You may do this simply by
        calling method [get] from the base class and passing to it string name of the
        lexem (for diagnostic purposes) and regular pattern of type [Str.regexp].
       } 
    }    

    [matcher] refers to two predefined types: [Msg.t] and [Token.t]. If you are
    not satisfied with them please consider implementing your own stream.   
*)

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
    scans [s] from [b] to [e] inclusively and shifts [loc] to take newlines into account
*)
val shiftPos : Msg.Coord.t -> string -> int -> int -> Msg.Coord.t

(** Matcher pattern to inherit from to obtain the steream implementation. 
    [matcher s pos loc] creates an object that helps to match regular
    expressions against string [s] starting from position [pos]. [loc] is the text coordinate of the 
    position [pos]
*)
class virtual matcher : string -> int -> Msg.Coord.t ->
  object ('a)

    (** [get name expr] is a parse function which parses regular expression [expr] at the current
        position. [name] is a name for diagnostic purposes
    *)
    method get : string -> Str.regexp -> ('a, Token.t, Msg.t) Ostap.result

    (** [getEOF] detects end of stream. *)
    method getEOF : ('a, Token.t, Msg.t) Ostap.result

    (** [getFIRST] gets an empty token at the current position and serves to obtain
        coordinate of the first symol of current stream
    *)
    method getFIRST : ('a, Token.t, Msg.t) Ostap.result

    (** [getLAST] gets an empty token at the  current position and serves to obtain
        coordinate of last symbol  
    *)
    method getLAST : ('a, Token.t, Msg.t) Ostap.result

    (** [look str] looks at the current stream for string [str] *)
    method look : string -> ('a, Token.t, Msg.t) Ostap.result

    (** Virtual method to skip meaningless symbols (e.g. whitespaces); returns
        position and coordinates of first meaningful symbol. [skip] is implicitly
        called prior to all of the above methods except for the [getLAST]
    *)
    method virtual skip : int -> Msg.Coord.t -> int * Msg.Coord.t

  end
