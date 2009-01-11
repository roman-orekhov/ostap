(*
 * Pa_ostap: a camlp4 extension to wrap Ostap's combinators.
 * Copyright (C) 2006-2009
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

(**/**)

open Camlp4.PreCast
open Syntax
open Print

module TeX =
  struct

    let enabled = ref false

    let opt   str   = sprintf "\\bopt %s \\eopt" str
    let plus  str   = sprintf "%s\\niter" str
    let aster str   = sprintf "%s\\iter" str
    let group str   = sprintf "\\bgrp %s \\egrp" str
    let nt    str   = sprintf "\\nt{%s}" str
    let pnt   x y   = sprintf "\\pnt{%s}{%s}" x y
    let alt   lst   = List.fold_left (fun acc x -> (if acc = "" then "" else acc ^ "\\ralt ") ^ x) "" lst
    let seq   lst   = List.fold_left (fun acc x -> (if acc = "" then "" else acc ^ "\\rb ") ^ x) "" lst
    let list  f x   = List.fold_left (fun acc x -> (if acc = "" then "" else acc ^ ", ") ^ f x) "" x
    let args  str   = sprintf "[%s]" str
    let term  str   = sprintf "\\term{%s}" str
    let str   arg   = sprintf "\\term{%S}" arg
    let rule  x y   = sprintf "\\grule{%s}{%s\\rend}\n\n" x y
    let prule x y z = sprintf "\\prule{%s}{%s}{%s\\rend}\n\n" x y z
    let print str   = if !enabled then fprintf stderr "%s" str else ()

    let hash        = (Hashtbl.create 1024 : Hashtbl.t string string)
    let connect x y = Hashtbl.add hash x y
    let protect x   = try Hashtbl.find hash x with Not_found -> x

  end

module P = Camlp4.Printers.OCaml.Make (Camlp4.PreCast.Syntax)

let exprString e =
  let buf = Buffer.create 256 in
  let _   = Format.bprintf buf "%a@?" (new P.printer ())#expr e in
  let str = Printf.sprintf "%S" (Buffer.contents buf) in       
  String.sub str 1 ((String.length str) - 2) 

let pattString e =
  let buf = Buffer.create 256 in
  let _   = Format.bprintf buf "%a@?" (new P.printer ())#patt e in
  let str = Printf.sprintf "%S" (Buffer.contents buf) in       
  String.sub str 1 ((String.length str) - 2) 

EXTEND Gram
  GLOBAL: expr patt str_item;

  str_item: LEVEL "top" [
    [ "rules"; rules=y_rules; "end" -> <:str_item< value rec $list:rules$ >> ] 
  ];

  expr: BEFORE ";" [
    [ "rule"; (p, image)=y_alternatives; "end" ->
        let rule = <:expr< fun [ s -> $p$ s ] >> in
        do {
          if TeX.enabled.val
  	  then TeX.connect (exprString rule) image
	  else ();
	  TeX.print image;
          rule
        } 
    ] |
    [ "let"; "rules"; "="; rules=y_rules; "end"; "in"; e=expr -> 
        <:expr< let rec $list:rules$ in $e$ >>
    ] 
  ];

  y_rules: [
    [ rules=LIST1 y_rule SEP ";" ->
      List.map
	(fun (name, args, rule) -> 
	  match args with [
	    None -> <:binding< $lid:name$  = $rule$ >>
	
	  | Some args ->
	      let pfun = <:expr< fun [ $args$ -> $rule$ ] >> in
	      <:binding< $lid:name$ = $pfun$ >>
          ]
	) 
	rules
    ]
  ];

  y_rule: [
    [ name=LIDENT; args=OPT y_formal_parameters; ":"; (p, image)=y_alternatives ->
      let rule = <:expr< fun [ s -> $p$ s ] >> in
      do {
        match args with [
	  None   -> TeX.print (TeX.rule name image)
	| Some p ->
	    let p =
	      if TeX.enabled.val 
	      then pattString p
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
	let (p, images) = List.split p in	
	let s = 
	  match s with [
	    Some s -> s
	  | None -> 
	      let (tuple, _) =
		List.fold_right 
		  (fun (_, omit, _, _) ((acc, i) as x) -> 
		    if omit then x else ([ <:expr< $lid:"_" ^ (string_of_int i)$>> :: acc ], i+1)
		  ) 
		  p 
		  ([], 0) 
	      in
	      match tuple with [
		[]  -> <:expr< () >>
	      | [x] -> x
	      |  _  -> <:expr< ( $tup:Ast.exCom_of_list tuple$ ) >> 
	      ]

	  ]
	in
        match List.fold_right
            (fun (flag, omit, binding, p) rightPart -> 
	      let p =
		match flag with [
	          None -> p
		| Some f -> 
		    let (patt, body) = 
		      match binding with [
			None   -> (<:patt< _   >>, f)
		      | Some p -> (<:patt< $p$ >>, f)
	              ]
		    in
		    let pfun = <:expr< fun [ $patt$ -> $body$ ] >> in
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
	      let sfun      = <:expr< fun [ $patt$ -> $right$ ] >> in
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
    [ (e, s)=y_postfix; "*" -> (<:expr< Ostap.many $e$ >>, TeX.aster s) ] |
    [ (e, s)=y_postfix; "+" -> (<:expr< Ostap.some $e$ >>, TeX.plus  s) ] |
    [ (e, s)=y_postfix; "?" -> (<:expr< Ostap.opt  $e$ >>, TeX.opt   s) ] | 
    [ y_primary ] 
  ];

  y_primary: [
    [ (p, s)=y_reference; args=OPT y_parameters -> 
          match args with [
             None           -> (p, TeX.nt s)
           | Some (args, a) -> (List.fold_left (fun expr arg -> <:expr< $expr$ $arg$ >>) p args, TeX.pnt s a)
          ]
    ] |
    [ p=UIDENT ->            
          let p' = "get" ^ p in
          let look = <:expr< s # $p'$ >> in
          (<:expr<fun [ s -> $look$ ]>>, TeX.term p)
    ] |
    [ p=STRING -> 
          let look = <:expr< s # look $str:p$ >> in
          (<:expr<fun [ s -> $look$ ] >>, TeX.str p)
    ] |
    [ "("; (p, s)=y_alternatives; ")" -> (p, TeX.group s) ] 
  ];

  y_reference: [
    [ (p, s)=y_path -> (p, s) ] |
    [ "!"; (p, s)=y_qualified -> (p, s) ]
  ];

  y_qualified: [
    [ y_path ] | 
    [ q=UIDENT; "."; (p, s)=y_qualified  -> 
        let i = <:ident< $uid:q$ >> in 
        let e = <:expr< $id:i$ >> in
        (<:expr< $e$.$p$ >>, sprintf "%s.%s" q s) 
    ]
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
      then (e, TeX.list (fun e -> TeX.protect (exprString e)) e) 
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

Camlp4.Options.add "-tex" (Arg.Set TeX.enabled) " - print TeX documentation to stderr";
