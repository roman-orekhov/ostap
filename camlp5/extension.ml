(*
 * Extension: a camlp5 extension to wrap Ostap's combinators.
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

(** Pa_ostap --- a camlp5 syntax extension for BNF-like grammar definitions *)

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

open BNF3;

module Args =
  struct
    
    value (h : Hashtbl.t string string) = Hashtbl.create 1024;

    value register x = Hashtbl.add h x x;
    value wrap     x = 
      try Expr.custom [`S (Hashtbl.find h x)] with [
	Not_found -> Expr.nonterm x
      ];
    value clear () = Hashtbl.clear h;

  end;

module Cache =
  struct

    value (h : Hashtbl.t string Expr.t) = Hashtbl.create 1024;

    value compress x =
      let b = Buffer.create 1024 in
      do {
        let f = ref False in
        for i=0 to String.length x - 1 do {
          match x.[i] with [
	    ' ' -> if f.val then () else do {Buffer.add_char b ' '; f.val := True}
	  | '\t' | '\n' -> f.val := False
	  | c -> do {Buffer.add_char b c; f.val := False}
	  ]
        };
        Buffer.contents b
      };

    value cache x y = Hashtbl.add h (compress x) y;

    value rec cached x = 
      let x = compress x in
      let rec substitute acc s i j = 
	let len = String.length s in
	if j < i then 
	  if i < len then substitute acc s (i+1) (len-1) else List.rev [`S s :: acc]
        else if i = len then List.rev [`S s :: acc]
             else 
	       let d = String.sub s i (j-i+1) in 
	       try 
		 substitute 
		   [`T (Hashtbl.find h d) :: [`S (String.sub s 0 i) :: acc]] 
		   (String.sub s (j + 1) (len - j - 1))
		   0
		   (len - j - 2)		 
	       with [
		 Not_found -> substitute acc s i (j-1)
	       ]	     
      in 
      match substitute [] x 0 (String.length x - 1) with [
	[`S s] -> Args.wrap s
      | list   -> Expr.custom list
      ];

  end;
 
value printBNF  = ref (fun (_: option string) (_: string) -> ());
value printExpr = ref (fun (_: MLast.expr) -> "");
value printPatt = ref (fun (_: MLast.patt) -> "");

value texDef     def  = Def.toTeX def;
value texDefList defs =
  let buf = Buffer.create 1024 in
  do {
    List.iter (fun def -> Buffer.add_string buf (sprintf "%s\n" (Def.toTeX def))) defs;
    Buffer.contents buf
  };

value bindOption x f =
  match x with [
    None -> None
  | Some x -> Some (f x)
  ]
;

EXTEND
  GLOBAL: expr patt str_item let_binding; 

  doc_name: [ [ "["; name=STRING; "]" -> name ] ];

  str_item: LEVEL "top" [
    [ "ostap"; doc=OPT doc_name; "("; rules=o_rules; ")" -> 
      let (rules, defs) = rules in
      do {
	printBNF.val doc (texDefList defs);
        <:str_item< value $opt:True$ $list:rules$ >> 
      }
    ] 
  ];

  let_binding: [
    [ "ostap"; doc=OPT doc_name; "("; rule=o_rule; ")" -> 
      let ((name, rule), def) = rule in 
      do {
	printBNF.val doc (texDef def);
        (<:patt< $lid:name$ >>, rule) 
      }
    ] 
  ];

  expr: LEVEL "top" [
    [ "ostap"; "("; (p, tree)=o_alternatives; ")" ->
      let body = <:expr< $p$ s >> in
      let pwel = [(<:patt< s >>, Ploc.VaVal None, body)] in
      do {
	let f = <:expr< fun [$list:pwel$] >> in
	match tree with [Some tree -> Cache.cache (printExpr.val f) tree | None -> ()];
        f
      }
    ] |
    [ "let"; "ostap"; doc=OPT doc_name; "("; rules=o_rules; ")"; "in"; e=expr LEVEL "top" ->
      let (rules, defs) = rules in
      do {
	printBNF.val doc (texDefList defs);
        <:expr< let $opt:True$ $list:rules$ in $e$ >> 
      }
    ] 
  ];

  o_rules: [
    [ rules=LIST1 o_rule SEP ";" ->      
      let (rules, defs) = List.split rules in
      (List.map	(fun (name, rule) -> (<:patt< $lid:name$ >>, rule)) rules, defs)
    ]
  ];

  o_rule: [
    [ name=LIDENT; args=OPT o_formal_parameters; ":"; (p, tree)=o_alternatives ->
      let args' = 
	match args with [
	  None   -> [<:patt< s >>]
	| Some l -> [<:patt< s >> :: l]
	]
      in
      let rule =
	List.fold_right 
	  (fun x f -> 
	    let pwel = [(x, Ploc.VaVal None, f)] in
	    <:expr< fun [$list:pwel$] >>
	  ) 
	  args'
	  <:expr< $p$ s >>
      in
      do {
	let p =
	  match args with [
	    None      -> []
          | Some args -> List.map printPatt.val args
	  ]
	in
	let tree =
	  match tree with [
	    None      -> Expr.string ""
	  | Some tree -> tree
	  ]
	in
	let def = 
          match p with [
	    []   -> Def.make  name tree
	  | args -> Def.makeP name args tree
	  ]
	in
        Args.clear ();
        ((name, rule), def)
      }
    ]
  ];

  o_formal_parameters: [ [ p=LIST1 o_formal_parameter -> p ]];

  o_formal_parameter: [
    [ "["; p=patt; "]" ->
      do {
         let rec get_defined_ident = fun [
	     <:patt< $_$ . $_$ >> -> []
           | <:patt< _ >> -> []
	   | <:patt< $lid:x$ >> -> [x]
	   | <:patt< ($p1$ as $p2$) >> -> get_defined_ident p1 @ get_defined_ident p2
	   | <:patt< $int:_$ >> -> []
	   | <:patt< $flo:_$ >> -> []
	   | <:patt< $str:_$ >> -> []
	   | <:patt< $chr:_$ >> -> []
	   | <:patt< [| $list:pl$ |] >> -> List.flatten (List.map get_defined_ident pl)
	   | <:patt< ($list:pl$) >> -> List.flatten (List.map get_defined_ident pl)
	   | <:patt< $uid:_$ >> -> []
	   | <:patt< ` $_$ >> -> []
	   | <:patt< # $list:_$ >> -> []
	   | <:patt< $p1$ $p2$ >> -> get_defined_ident p1 @ get_defined_ident p2
	   | <:patt< { $list:lpl$ } >> ->
	       List.flatten (List.map (fun (lab, p) -> get_defined_ident p) lpl)
	   | <:patt< $p1$ | $p2$ >> -> get_defined_ident p1 @ get_defined_ident p2
	   | <:patt< $p1$ .. $p2$ >> -> get_defined_ident p1 @ get_defined_ident p2
	   | <:patt< ($p$ : $_$) >> -> get_defined_ident p
	   | <:patt< ~$_$ >> -> []
	   | <:patt< ~$_$: $p$ >> -> get_defined_ident p
	   | <:patt< ?$_$ >> -> []
	   | <:patt< ?$_$: ($p$) >> -> get_defined_ident p
	   | <:patt< ?$_$: ($p$ = $e$) >> -> get_defined_ident p
	   | <:patt< $anti:p$ >> -> get_defined_ident p
	   | _ -> [] 
         ]
	 in	
         List.iter Args.register (get_defined_ident p);
	 p
      }
    ]
  ];

  o_alternatives: [
    [ p=LIST1 o_alternativeItem SEP "|" -> 
        match p with [
	  [p] -> p
        |  _  -> 
	    let (p, trees) = List.split p in
	    let trees =
	      List.map
		(fun x -> match x with [Some x -> x])
		(List.filter (fun x -> x <> None) trees)
	    in
	    match
	      List.fold_right 
		(fun item expr -> 
		  match expr with [
		    None -> Some (item)
		  | Some expr -> Some (<:expr< Ostap.alt $item$ $expr$ >>)
	          ]
		) p None
	    with [
	      None   -> raise (Failure "internal error - must not happen")
	    | Some x -> (x, match trees with [[] -> None | _ -> Some (Expr.alt trees)])
	    ]
	]
    ]
  ];

  o_alternativeItem: [
    [ g=OPT o_guard; p=LIST1 o_prefix; s=OPT o_semantic -> 
	let (p, trees) = List.split p in
	let trees = 
	  List.map 
	    (fun x -> match x with [Some x -> x]) 
	    (List.filter (fun x -> x <> None) trees) 
	in
	let trees = 
	  match trees with [
	    [] -> None
	  | _  -> Some (Expr.seq trees)
	  ]
	in
	let (s, isSema) = 
	  match s with [
	    Some s -> (s, True)
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
		[]  -> (<:expr< () >>, True)
	      | [x] -> (x, False)
	      |  _  -> (<:expr< ($list:tuple$) >>, True)
	      ]

	  ]
	in
        match List.fold_right
            (fun (flag, omit, binding, p) rightPart -> 
	      let p =
		match flag with [
	          None -> p
		| Some (f, r) -> 
		    let pwel = 
		      match binding with [
			None   -> [(<:patt< _ >>, Ploc.VaVal None, f)] 
		      | Some p -> [(<:patt< $p$ >>, Ploc.VaVal None, f)]
	              ]
		    in
		    let pfun = <:expr< fun [$list:pwel$] >> in
		    match r with [
		      None   -> <:expr< Ostap.guard $p$ $pfun$ None >>
		    | Some r -> 
			let pwel = 
			  match binding with [
			    None   -> [(<:patt< _ >>, Ploc.VaVal None, r)] 
			  | Some p -> [(<:patt< $p$ >>, Ploc.VaVal None, r)]
	                  ]
			in
			let rfun = <:expr< fun [$list:pwel$] >> in
			<:expr< Ostap.guard $p$ $pfun$ (Some $rfun$) >>
	            ]
	        ]
	      in
	      let (n, right, combi, isMap) = 
		match rightPart with [
		  None -> (0, s, (fun x y -> <:expr< Ostap.map $y$ $x$>>), True)
		| Some (right, n) -> (n, right, (fun x y -> <:expr< Ostap.seq $x$ $y$>>), False)
	      ]
	      in
	      if not isSema && not omit && isMap && binding = None
	      then Some (p, n+1)
	      else 
		let patt = match binding with [None -> <:patt< _ >> | Some patt -> patt] in 
		let (patt, n) = if not omit then (<:patt< ($patt$ as $lid:"_" ^ (string_of_int n)$) >>, n+1) else (patt, n) in
		let pwel      = [(patt, Ploc.VaVal None, right)] in
		let sfun      = <:expr< fun [$list:pwel$] >> in
		Some (combi p sfun, n)
            ) p None
	with [
	  Some (expr, _) -> 
	    match g with [
	      None   -> (expr, trees)
	    | Some (g, None) ->
		(<:expr< Ostap.seq (Ostap.guard Ostap.empty (fun _ -> $g$) None) (fun _ -> $expr$) >>, trees)

	    | Some (g, Some r) ->
		(<:expr< Ostap.seq (Ostap.guard Ostap.empty (fun _ -> $g$) (Some (fun _ -> $r$))) (fun _ -> $expr$) >>, trees)
	    ]
	| None -> raise (Failure "internal error: empty list must not be eaten")
	]
    ] 
  ];

  o_prefix: [
    [ m=OPT "-"; (p, s)=o_basic -> 
       let (binding, parse, f) = p in
       ((f, (m <> None), binding, parse), s)
    ]
  ];

  o_basic: [
    [ p=OPT o_binding; (e, s)=o_postfix; f=OPT o_predicate -> ((p, e, f), s) ]
  ];

  o_postfix: [
    [ o_primary ] |
    [ (e, s)=o_postfix; "*" -> (<:expr< Ostap.many $e$ >>, bindOption s (fun s -> Expr.star s)) ] |
    [ (e, s)=o_postfix; "+" -> (<:expr< Ostap.some $e$ >>, bindOption s (fun s -> Expr.plus s)) ] |
    [ (e, s)=o_postfix; "?" -> (<:expr< Ostap.opt  $e$ >>, bindOption s (fun s -> Expr.opt  s)) ] |
    [ (e, s)=o_postfix; "::"; "("; c=expr; ")" -> (<:expr< Ostap.comment $e$ ($c$) >>, s) ]
  ];

  o_primary: [
    [ (p, s)=o_reference; args=OPT o_parameters -> 
          match args with [
             None           -> (p, Some s)
           | Some (args, a) -> 
	       let args = [<:expr< s >> :: args] in
	       let body = List.fold_left (fun expr arg -> <:expr< $expr$ $arg$ >>) p args in
	       let pwel = [(<:patt< s >>, Ploc.VaVal None, body)] in
	       (<:expr< fun [$list:pwel$] >>, (Some (Expr.apply s a)))
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
            (<:expr< fun [$list:pwel$] >>, Some (Expr.term p))
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
          (<:expr<fun [$list:pwel$]>>, Some (Expr.string p))
    ] |
    [ "$"; "("; p=expr; ")" ->
          let look = <:expr< s # look ($p$) >> in
          let pwel = [
	    (
	     <:patt<$lid:"s"$>>, 
	     Ploc.VaVal None, 
	     look 
	    )
	  ] in
          (<:expr<fun [$list:pwel$]>>, Some (Expr.string (printExpr.val p)))
    ] |
    [ "$" -> (<:expr< Ostap.lift >>, None) ] |
    [ "("; (p, s)=o_alternatives; ")" -> (p, bindOption s (fun s -> Expr.group s)) ]   
  ];

  o_reference: [
    [ p=LIDENT -> (<:expr< $lid:p$ >>, Args.wrap p ) ] |
    [ "!"; "("; e=expr; ")" -> (e, Expr.string (printExpr.val e)) ]
  ];

  o_parameters: [ [ p=LIST1 o_parameter -> List.split p ]];

  o_parameter: [ [ "["; e=expr; "]" -> (e, Cache.cached (printExpr.val e))] ];

  o_binding: [ 
    [ "<"; p=patt; ">"; ":" -> p ] |
    [ p=LIDENT; ":" -> <:patt< $lid:p$ >> ]
  ];

  o_semantic: [ ["{"; e=expr; "}" -> e ] ];

  o_predicate: [ [ "=>"; e=o_guard -> e ] ];

  o_guard: [ [ "{"; e=expr; "}"; r=OPT o_reason; "=>" -> (e, r) ] ];

  o_reason: [ [ "::"; "("; e=expr; ")" -> e ] ];

END;

add_option "-tex"  (Arg.String 
		      (fun s -> 
			 do {
		  	   let p = printBNF.val in

			   let ouch = open_out (s ^ ".tex") in

			   close_out ouch;

                           printExpr.val := (fun e -> Eprinter.apply pr_expr Pprintf.empty_pc e);
			   printPatt.val := (fun p -> Eprinter.apply pr_patt Pprintf.empty_pc p);

			   printBNF .val := 
			     (fun name bnf -> 
			        do {	                          
                                  let ouch = 
				    match name with [
				      None      -> open_out_gen [Open_append; Open_text] 0o66 (s ^ ".tex")
				    | Some name -> open_out (s ^ "." ^ name ^ ".tex") 
	                            ]
				  in
			          fprintf ouch "%s" bnf; 
			          close_out ouch;
			          p name bnf
                                }
			     )
                        }
		      )
		   ) 
           "<name> - print TeX grammar documentation to given file";
