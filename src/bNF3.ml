(*
 * BNF3: BNF tree representation.
 * Copyright (C) 2008
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

open Printf

module TeX =
  struct

    let quote s =
      let buf = Buffer.create (String.length s * 2) in
      for i=0 to String.length s - 1 do
	Buffer.add_string buf
	  (match s.[i] with
	  | '{'  -> "\\{"
	  | '}'  -> "\\}"
	  | '$'  -> "\\$"
	  | '&'  -> "\\&"
	  | '#'  -> "\\#"
	  | '%'  -> "\\%"
	  | '_'  -> "\\_"
	  | '~'  -> "$\\tilde{}$"
	  | '\\' -> "$\\backslash$"
	  | '<'  -> "$<$"
	  | '>'  -> "$>$"
	  | '|'  -> "$|$"
	  | '^'  -> "$\\hat{}$"
	  | c    -> String.make 1 c
	  )
      done;
      Buffer.contents buf 
    
    let opt    str   = sprintf "\\bopt %s \\eopt" str
    let plus   str   = sprintf "%s\\niter" str
    let aster  str   = sprintf "%s\\iter" str
    let group  str   = sprintf "\\bgrp %s \\egrp" str
    let nt     str   = sprintf "\\nt{%s}" (quote str)
    let pnt    x y   = sprintf "\\pnt{%s}{%s}" (quote x) y
    let alt    lst   = List.fold_left (fun acc x -> (if acc = "" then "" else acc ^ "\\ralt ") ^ x) "" lst
    let seq    lst   = List.fold_left (fun acc x -> (if acc = "" then "" else acc ^ "\\rb ") ^ x) "" lst
    let list   f x   = List.fold_left (fun acc x -> (if acc = "" then "" else acc ^ ", ") ^ f x) "" x
    let args   str   = sprintf "\\args{%s}" str
    let term   str   = sprintf "\\term{%s}" (quote str)
    let str    arg   = sprintf "\\term{%s}" (quote arg)
    let rule   x y   = sprintf "\\grule{%s}{%s\\rend}\n\n" x y
    let prule  x y z = sprintf "\\prule{%s}{%s}{%s\\rend}\n\n" x y z
    let custom x     = quote x

  end

module Expr =
  struct

    type t =
	String  of string
      | Term    of string
      | Nonterm of string
      | Apply   of t * t list
      | Star    of t
      | Plus    of t
      | Opt     of t
      | Alt     of t list
      | Seq     of t list
      | Group   of t
      | Custom  of string

    let rec toTeX = function      
      |	String   s     -> TeX.str   s
      | Term     t     -> TeX.term  t
      | Nonterm  n     -> TeX.nt    n
      | Star     e     -> TeX.aster  (toTeX e)
      | Plus     e     -> TeX.plus   (toTeX e)
      | Opt      e     -> TeX.opt    (toTeX e)
      | Alt      l     -> TeX.alt    (List.map toTeX l)
      | Seq      l     -> TeX.seq    (List.map toTeX l)
      | Group    e     -> TeX.group  (toTeX e)
      | Custom   s     -> TeX.custom  s

      | Apply   (Nonterm x, y) -> TeX.pnt x (TeX.list toTeX y)
      | Apply   (x, y)         -> sprintf "%s%s" (toTeX x) (TeX.args (TeX.list toTeX y))
	    
  end

module Def =
  struct

    type t = string * string option * Expr.t

    let make  name expr     = name, None, expr
    let makeP name arg expr = name, Some arg, expr

    let rec toTeX (name, args, expr) =
      match args with
      | None      -> TeX.rule  name (Expr.toTeX expr)
      | Some args -> TeX.prule name args (Expr.toTeX expr)      

  end
