(*
 * Pa_yard: a camlp4 extension to wrap Ostap's combinators.
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

(**/**)

#load "pa_extend.cmo";
#load "q_MLast.cmo";

open Pcaml;

EXTEND
  GLOBAL: expr patt str_item;

  str_item: LEVEL "top" [
    [ "rules"; rules=y_rules; "end" -> <:str_item< value $opt:True$ $list:rules$ >> ] 
  ];

  expr: LEVEL "top" [
    [ "rule"; p=y_alternatives; "end" ->
      let body = <:expr< $p$ s >> in
      let pwel = [(<:patt< s >>, None, body)] in
      <:expr< fun [$list:pwel$] >>
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
	      let pwel = [(args, None, rule)] in
	      let pfun = <:expr< fun [$list:pwel$] >> in
	      (<:patt< $lid:name$ >>, pfun)
          ]
	) 
	rules 
    ]
  ];

  y_rule: [
    [ name=LIDENT; args=OPT y_formal_parameters; ":"; p=y_alternatives ->
      let body = <:expr< $p$ s >> in
      let pwel = [(<:patt< s >>, None, body)] in
      let rule = <:expr< fun [$list:pwel$] >> in
      (name, args, rule)
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
	    | Some x -> x
	    ]
	]
    ]
  ];

  y_alternativeItem: [
    [ p=LIST1 y_prefix; s=OPT y_semantic -> 
        let items = List.length p in
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
			None   -> [(<:patt< _ >>, None, f)] 
		      | Some p -> [(<:patt< $p$ >>, None, f)]
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
	      let pwel      = [(patt, None, right)] in
	      let sfun      = <:expr< fun [$list:pwel$] >> in
	      Some (combi p sfun, n)
            ) p None
	with [
	  Some (expr, _) -> expr
	| None -> raise (Failure "internal error: empty list must not be eaten")
	]
    ] 
  ];

  y_prefix: [
    [ m=OPT "-"; p=y_basic -> 
       let (binding, parse, f) = p in
       (f, (m <> None), binding, parse)
    ]
  ];

  y_basic: [
    [ p=OPT y_binding; e=y_postfix; f=OPT y_predicate -> (p, e, f) ]
  ];

  y_postfix: [
    [ y_primary ] |
    [ e=y_postfix; "*" -> <:expr< Ostap.many $e$ >> ] |
    [ e=y_postfix; "+" -> <:expr< Ostap.some $e$ >> ] |
    [ e=y_postfix; "?" -> <:expr< Ostap.opt $e$ >> ]
  ];

  y_primary: [
    [ p=LIDENT; args=OPT y_parameters -> 
          match args with [
             None      -> <:expr<$lid:p$>>
           | Some args -> 
               let p = <:expr<$lid:p$>> in
	       List.fold_left (fun expr arg -> <:expr< $expr$ $arg$ >>) p args
          ]
    ] |
    [ p=UIDENT ->  
          do {
            let p = "get" ^ p in
            let look = <:expr< s # $p$ >> in
            let pwel = [
	      (
	       <:patt< s >>, 
	       None, 
	       look
	      )
	    ] in
            <:expr<fun [$list:pwel$]>>
          }
    ] |
    [ p=STRING -> 
          let look = <:expr< s # look $str:p$ >> in
          let pwel = [
	    (
	     <:patt<$lid:"s"$>>, 
	     None, 
	     look 
	    )
	  ] in
          <:expr<fun [$list:pwel$]>>
    ] |
    [ "("; p=y_alternatives; ")" -> p ]   
  ];

  y_parameters: [
    [ "["; e=LIST1 expr; "]" -> e ]
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
