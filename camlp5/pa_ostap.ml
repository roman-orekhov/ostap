(*
 * Pa_ostap: a camlp4 extension to wrap Ostap's combinators.
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

(** Pa_ostap --- a camlp4 syntax extension for BNF-like grammar definitions *)

(**

  {2 General description}

  [Pa_ostap] extends Objective Caml grammar with two constructs: [rule] and [rules].
  The latter represents a set of (mutually recursive) {b grammar entries definitions}  
  at the structure level while the former denotes the single {b grammar expression}. Both 
  constructions are converted into pure OCaml using [Ostap] parser combinators.

  While [Ostap] is purely abstract with regard to stream implementation [Pa_ostap] 
  additionaly allows for convenient integration of parsing and lexing by considering {i streams
  as objects}. Namely, the stream of tokens [L]{_1}, [L]{_2}, ..., [L]{_k} is represented by an object
  with member functions [getL]{_1}, [getL]{_2}, ..., [getL]{_k}. Such a representation allows
  to freely combine various parser functions that operate on different types of streams with almost no
  type limitations at construction time.

  Additionally to this documentation we provide a closed example of how to use [Pa_ostap] (see
  [sample] directory of the distribution.

  {2 Grammar expression}

  The syntax of {b grammar expression} is as follows:
  
  [expr] {b :} [alternative]{_[1]} {b | } [alternative]{_[2]} {b | ... |} [alternative]{_[k]}

  [alternative] {b :} [prefixed+] {b \[ } [action]  {b \] }

  [prefixed] {b : } {b \[ } [-] {b \] } [basic]    

  [basic] {b : } {b \[ } [binding] {b \] } [postfix] {b \[ } [predicate] {b \]}

  [postfix] {b : } [primary] {b | } [postfix] {b ( } [*] {b | } [+] {b | } [?] {b ) }

  [primary] {b : } {i UIDENT} {b | } {i reference} {b \[ } [parameters] {b \] } {b | } {i STRING} {b | ( } [expr] {b )}

  [reference] {b : } {i LIDENT} {b | } {b !} [qualified]

  [qualified] {i LIDENT} {b | } {i UIDENT} {b .} [qualified]

  [parameters] {b : } [\[] {i EXPR} [\]]

  [binding] {b : } [<] {i PATT} [>=]

  [predicate] {b : } [=> {] {i EXPR}  [}=>]

  [action] {b : } [{] {i EXPR} [}]

  Here {i UIDENT} and {i LIDENT} stand for identifiers starting from uppercase and lowercase letters
  correspondingly, {i EXPR} --- for OCaml expression, {i PATT} --- for OCaml pattern.

  [reference] within grammar expression denotes a {i parse function} that applied to a stream to
  obtain parsed value and residual stream (see module [Ostap]). Each reference is either a {i LIDENT} or
  a qualified reference as per OCaml, prefixed by ! to distinguish from {i UIDENT}. 
  {i UIDENT} is treated as a lexeme reference;
  thought generally speaking parsing with Ostap does not require any lexer to be provided (you must instead supply
  a set of basic parse functions in any way you find convenient) [Pa_ostap] additionally operates with some predefined
  representation of streams as objects (see module [Matcher]). This representation does not interfere with the
  common approach and you need not use this feature unless you explicitly apply to it. There are only two constructions
  that refer to object implementation of streams: {i UIDENT} and {i STRING}. If you use {i UIDENT} in grammar 
  expression, for example {i NAME}, then the stream to parse with this expression has to provide a member function
  {i getNAME}. Similarly using {i STRING} in expression requires stream to provide a member {i look}. 

  We will not describe the meaning of all constructions in all details since generally it follows the common
  BNF style; instead we demonstrate some examples that cover all cases of their exploration. 

  {b Examples:}

  {ol
    {li ["(" expression ")"] is a grammar expression to define a function that matches a stream against successive 
     occurrences of ["("], that that parsed by [expression], and [")"]. On success this function returns {i a triple}:
     the token for ["("], the value parsed by [expression], and the token for [")"]. There are generally two ways
     to exclude ["("] and [")"] from the result. The first way is to bind the result of [expression] to some name 
     and then explicitly specify the result of grammar expression as follows:

     ["(" <e>=expression ")" {e}]
  
     The second is just to say to omit brackets:

     [-"(" expression -")"].

     Note that you may specify any pattern in the left part of binding; note also that you {i must not} split
     the symbol [">="] in binding specification (so, [<e> =expression] won't be parsed by [Pa_ostap]). Prefix
     omitting operator "[-]" may also be applied to any grammar expression, enclosed in brackets.
    }
    {li [<hd>=item <tl>=(-"," item)* {hd :: tl}] defines a function to parse a list of items}
    {li [(<s>=string {`Str s} | <x>=integer {`Int x})*] defines a function to parse a list of strings or integers}
    {li [<hd>=integer <tl>=(-(","?) integer)* {hd :: tl}] parses a list of integers delimited by optional commas}
    {li [<x>=integer => {x > 0} => {x}] parses positive integer value}
    {li [<x>=(integer?) => {match x with Some 0 -> false | _ -> true} => {x}] parses optional non-zero integer value}    
    {li [<x>= ! MyParseLibrary.MyModule.parseIt] parses the source with parse function specified by qualified name}    
  }
 
  In all examples above we assume that [integer] parses integer value, [string] --- string value.
  Additionally you may specify parameters to parse functions used in grammar expression by
  enclosing them in square brackets.

  {2 Rule definition}

  [rule] construction serves to embed a single grammar expression into the program code. The
syntax is as follows:

  [rule] {b : } {b rule} [expr] {b end}

  For example, the following code

  [let intPair = rule integer integer end]

  binds identifier [intPair] to parse function that parses and returns a pair of integers. Here
  assumed that [integer] is a parse function to parse integer literals.

  You may, of course, define a custom parser combinator: 

  [let inBrackets what = rule -"(" what -")" end]

  or

  [let listOf item delim = rule <hd>=item <tl>=(-delim item)* {hd :: tl} end]

  {2 Grammar entries}
 
  The following construction allows to define a set of mutually recursive grammar definitions
  at the structure (module implementation) level:

  [rules] {b : } {b rules} [entry]{_[1]} {b ;} [entry]{_[2]} {b ;} ... {b ;} [entry]{_[k]} {b end}

  [entry] {b : } {i LIDENT} {b \[ } arguments {b \] } {b : } [expr]

  [arguments] {b : } [\[] {i PATT} [\]]   

  For example,

  [rules]
 
  [   sequence[start]: item[start] | <next>=item[start] sequence[next];]

  [   item[start]: <x>=integer {x+start} | ";" {start}];

  [   entry: sequence[0]]

  [end]

  declares (among others) the parser function [entry] that parses and sums a semicolon-terminated 
  sequence of integers.
*)

(**/**)

#load "pa_extend.cmo";
#load "q_MLast.cmo";

open Pcaml;
open Printf;

module TeX =
  struct

    value enabled = ref False;

    value opt   str   = sprintf "\\bopt %s \\eopt" str;
    value plus  str   = sprintf "%s\\niter" str;
    value aster str   = sprintf "%s\\iter" str;
    value group str   = sprintf "\\bgrp %s \\egrp" str;
    value nt    str   = sprintf "\\nt{%s}" str;
    value pnt   x y   = sprintf "\\pnt{%s}{%s}" x y;
    value alt   lst   = List.fold_left (fun acc x -> (if acc = "" then "" else acc ^ "\\ralt ") ^ x) "" lst;
    value seq   lst   = List.fold_left (fun acc x -> (if acc = "" then "" else acc ^ "\\rb ") ^ x) "" lst;
    value list  f x   = List.fold_left (fun acc x -> (if acc = "" then "" else acc ^ ", ") ^ f x) "" x;
    value args  str   = sprintf "[%s]" str;
    value term  str   = sprintf "\\term{%s}" str;
    value str   arg   = sprintf "\\term{%S}" arg;
    value rule  x y   = sprintf "\\grule{%s}{%s\\rend}\n\n" x y;
    value prule x y z = sprintf "\\prule{%s}{%s}{%s\\rend}\n\n" x y z;
    value print str   = if enabled.val then fprintf stderr "%s" str else ();

    value hash        = (Hashtbl.create 1024 : Hashtbl.t string string);
    value connect x y = Hashtbl.add hash x y;
    value protect x   = try Hashtbl.find hash x with [Not_found -> x];

  end;
 
EXTEND
  GLOBAL: expr patt str_item;

  str_item: LEVEL "top" [
    [ "rules"; rules=y_rules; "end" -> <:str_item< value $opt:True$ $list:rules$ >> ] 
  ];

  expr: LEVEL "top" [
    [ "rule"; (p, image)=y_alternatives; "end" ->
      let body = <:expr< $p$ s >> in
      let pwel = [(<:patt< s >>, Ploc.VaVal None, body)] in
      do {
        let rule = <:expr< fun [$list:pwel$] >> in
        if TeX.enabled.val
	then TeX.connect (Eprinter.apply pr_expr Pprintf.empty_pc rule) image
	else ();
	TeX.print image;
        rule
      }
    ] |
    [ "let"; "rules"; "="; rules=y_rules; "end"; "in"; e=expr LEVEL "top" ->       
      <:expr< let $opt:True$ $list:rules$ in $e$ >>
    ] 
  ];

  y_rules: [
    [ rules=LIST1 y_rule SEP ";" ->
      List.map
	(fun (name, args, rule) -> 
	  match args with [
	    None -> (<:patt< $lid:name$ >>, rule)
		
	  | Some args ->
	      let pwel = [(args, Ploc.VaVal None, rule)] in
	      let pfun = <:expr< fun [$list:pwel$] >> in
	      (<:patt< $lid:name$ >>, pfun)
          ]
	) 
	rules 
    ]
  ];

  y_rule: [
    [ name=LIDENT; args=OPT y_formal_parameters; ":"; (p, image)=y_alternatives ->
      let body = <:expr< $p$ s >> in
      let pwel = [(<:patt< s >>, Ploc.VaVal None, body)] in
      let rule = <:expr< fun [$list:pwel$] >> in
      do {
        match args with [
	  None      -> TeX.print (TeX.rule name image)
	| Some p ->
	    let p =
	      if TeX.enabled.val 
	      then Eprinter.apply pr_patt Pprintf.empty_pc p
	      else ""
	    in
	    TeX.print (TeX.prule name p image)
	];        
        (name, args, rule)
      }
    ]
  ];

  y_formal_parameters: [
    [ "["; p=LIST1 patt; "]" -> 
       match p with [
	 [hd] -> hd
       | [h::t] -> List.fold_left (fun acc p -> <:patt< $acc$ $p$ >>) h t
       ]
    ]
  ];

  y_alternatives: [
    [ p=LIST1 y_alternativeItem SEP "|" -> 
        match p with [
	  [p] -> p
        |  _  -> 
	    let (p, images) = List.split p in
	    match
	      List.fold_right 
		(fun item expr -> 
		  match expr with [
		    None -> Some (item)
		  | Some expr -> Some (<:expr< Ostap.alt $item$ $expr$ >>)
	          ]
		) p None
	    with [
	      None -> raise (Failure "internal error - must not happen")
	    | Some x -> (x, TeX.alt images)
	    ]
	]
    ]
  ];

  y_alternativeItem: [
    [ p=LIST1 y_prefix; s=OPT y_semantic -> 
        let items = List.length p in
	let (p, images) = List.split p in	
	let s = 
	  match s with [
	    Some s -> s
	  | None -> 
	      let (tuple, _) =
		List.fold_right 
		  (fun (_, omit, _, _) ((acc, i) as x) -> 
		    if omit then x else ([<:expr< $lid:"_" ^ (string_of_int i)$>> :: acc], i+1)
		  ) 
		  p 
		  ([], 0) 
	      in
	      match tuple with [
		[]  -> <:expr< () >>
	      | [x] -> x
	      |  _  -> <:expr< ($list:tuple$) >>
	      ]

	  ]
	in
        match List.fold_right
            (fun (flag, omit, binding, p) rightPart -> 
	      let p =
		match flag with [
	          None -> p
		| Some f -> 
		    let pwel = 
		      match binding with [
			None   -> [(<:patt< _ >>, Ploc.VaVal None, f)] 
		      | Some p -> [(<:patt< $p$ >>, Ploc.VaVal None, f)]
	              ]
		    in
		    let pfun = <:expr< fun [$list:pwel$] >> in
		    <:expr< Ostap.guard $p$ $pfun$ >>
	        ]
	      in
	      let patt              = match binding with [None -> <:patt< _ >> | Some patt -> patt] in
	      let (n, right, combi) = 
		match rightPart with [
		  None -> (0, s, (fun x y -> <:expr< Ostap.map $y$ $x$>>))
		| Some (right, n) -> (n, right, (fun x y -> <:expr< Ostap.seq $x$ $y$>>))
	        ]
	      in
	      let (patt, n) = if not omit then (<:patt< ($patt$ as $lid:"_" ^ (string_of_int n)$) >>, n+1) else (patt, n) in
	      let pwel      = [(patt, Ploc.VaVal None, right)] in
	      let sfun      = <:expr< fun [$list:pwel$] >> in
	      Some (combi p sfun, n)
            ) p None
	with [
	  Some (expr, _) -> (expr, TeX.seq images)
	| None -> raise (Failure "internal error: empty list must not be eaten")
	]
    ] 
  ];

  y_prefix: [
    [ m=OPT "-"; (p, s)=y_basic -> 
       let (binding, parse, f) = p in
       ((f, (m <> None), binding, parse), s)
    ]
  ];

  y_basic: [
    [ p=OPT y_binding; (e, s)=y_postfix; f=OPT y_predicate -> ((p, e, f), s) ]
  ];

  y_postfix: [
    [ y_primary ] |
    [ (e, s)=y_postfix; "*" -> (<:expr< Ostap.many $e$ >>, TeX.aster s) ] |
    [ (e, s)=y_postfix; "+" -> (<:expr< Ostap.some $e$ >>, TeX.plus  s) ] |
    [ (e, s)=y_postfix; "?" -> (<:expr< Ostap.opt $e$  >>, TeX.opt   s) ]
  ];

  y_primary: [
    [ (p, s)=y_reference; args=OPT y_parameters -> 
          match args with [
             None           -> (p, TeX.nt s)
           | Some (args, a) -> (List.fold_left (fun expr arg -> <:expr< $expr$ $arg$ >>) p args, TeX.pnt s a)
          ]
    ] |
    [ p=UIDENT ->  
          do {
            let p' = "get" ^ p in
            let look = <:expr< s # $p'$ >> in
            let pwel = [
	      (
	       <:patt< s >>, 
	       Ploc.VaVal None, 
	       look
	      )
	    ] in
            (<:expr<fun [$list:pwel$]>>, TeX.term p)
          }
    ] |
    [ p=STRING -> 
          let look = <:expr< s # look $str:p$ >> in
          let pwel = [
	    (
	     <:patt<$lid:"s"$>>, 
	     Ploc.VaVal None, 
	     look 
	    )
	  ] in
          (<:expr<fun [$list:pwel$]>>, TeX.str p)
    ] |
    [ "("; (p, s)=y_alternatives; ")" -> (p, TeX.group s) ]   
  ];

  y_reference: [
    [ (p, s)=y_path -> (p, s) ] |
    [ "!"; (p, s)=y_qualified -> (p, s) ]
  ];

  y_qualified: [
    [ y_path ] |
    [ q=UIDENT; "."; (p, s)=y_qualified -> (<:expr< $uid:q$.$p$ >>, sprintf "%s.%s" q s) ]
  ];

  y_path: [
    [ p=LIDENT; (t, s)=y_tail -> 
      match t with [
        `Empty    -> (<:expr< $lid:p$ >>          , p)
      | `Field  q -> (<:expr< $lid:p$ . $q$ >>    , sprintf "%s.%s" p s)
      | `Method q -> (<:expr< $lid:p$ # $lid:q$ >>, sprintf "%s#%s" p s)
      ]
    ] 
  ];

  y_tail:[
    [ "."; (p, s)=y_path -> (`Field  p, sprintf ".%s" s) ] |
    [ "#";  p    =LIDENT -> (`Method p, sprintf "#%s" p) ] |
    [ -> (`Empty, "") ] 
  ];

  y_parameters: [
    [ "["; e=LIST1 expr; "]" -> 
      if TeX.enabled.val 
      then (e, TeX.list (fun e -> TeX.protect (Eprinter.apply pr_expr Pprintf.empty_pc e)) e) 
      else (e, "")
    ]
  ];

  y_binding: [
    [ "<"; p=patt; ">=" -> p ] 
  ];

  y_semantic: [
    ["{"; e=expr; "}" -> e ]
  ];

  y_predicate: [
    [ "=>"; "{"; e=expr; "}"; "=>" -> e ]
  ];

END;

add_option "-tex" (Arg.Set TeX.enabled) " - print TeX documentation to stderr";
