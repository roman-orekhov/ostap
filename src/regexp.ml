type 'a t = 
    Test   of string * ('a -> bool)
  | Aster  of 'a t
  | Plus   of 'a t
  | Opt    of 'a t
  | Alter  of 'a t list
  | Juxt   of 'a t list
  | Arg    of string * 'a t
  | BOS
  | EOS

let rec toText = 
  let ttl t   = Pretty.listByComma (List.map toText t) in
  let ptt n t = Pretty.plock (Pretty.string n) t in
  function
  | Test (s, _) -> ptt "Test"  (Pretty.string s)
  | Aster t     -> ptt "Aster" (toText t)
  | Plus  t     -> ptt "Plus"  (toText t)
  | Opt   t     -> ptt "Opt"   (toText t)
  | Alter tl    -> ptt "Alter" (ttl tl)
  | Juxt  tl    -> ptt "Juxt"  (ttl tl)
  | Arg  (s, t) -> ptt "Arg"   (Pretty.seq  [Pretty.string s; toText t])
  | BOS         -> Pretty.string "BOS"
  | EOS         -> Pretty.string "EOS"

let toString s = Pretty.toString (toText s)

module Diagram =
  struct

    open Printf
    open List

    type 'a expr = 'a t
    
    type 'a cond = If of string * ('a -> bool) | Else | EoS | BoS
    and  'a tran = 'a cond * string list * 'a t
    and  'a sort = State of 'a tran list | Ok | Back of 'a t option ref
    and  'a t    = 'a sort * int

    exception Duplicate of string

    let getId             = snd 
    let getDest (_, _, x) = x
    let getTrans          = function (State x, _) -> x | _ -> [] 
    let setTrans y        = function (State x, i) -> State y, i | _ -> invalid_arg "Ostap.Regexp.Diagram.setTrans" 

    let toDOT root =
      let buf = Buffer.create 512 in
      Buffer.add_string buf "digraph X {\n";
      let node id label = Buffer.add_string buf (sprintf "node%d [label=\"%s\"];\n" id label) in      
      let edge id = 
        let doit t l =
          let rec derive = function
            | Back r, _ -> let Some t = !r in derive t
            | t -> t
          in      
          let inDOT i j label =
            Buffer.add_string buf (sprintf "node%d -> node%d [label=\"%s\"];\n" i j label)
          in
          inDOT id (getId (derive t)) l
        in
        let bindings =
          let module L = View.List (View.String) in
          L.toString
        in
        function
        | If (s, _), bs, t -> doit t (sprintf "if(%s)[%s]" s (bindings bs))
        | Else     , bs, t -> doit t (sprintf "else [%s]" (bindings bs))
        | EoS      , _ , t -> doit t "EoS" 
        | BoS      , _ , t -> doit t "BoS"
      in
      let rec inner (sort, id) as t =
        match sort with
        | State trans -> 
            node id "state";
            iter (fun tran -> edge id tran; inner (getDest tran)) trans
           
        | Ok -> node id "ok"

        | Back _ -> ()
      in
      inner root;
      Buffer.add_string buf "}\n";
      Buffer.contents buf

    let make expr =
      let checkName =
        let module S = Set.Make (String) in 
        let names    = ref S.empty in
        fun name -> 
          if S.mem name !names then raise (Duplicate name) else names := S.add name !names; name       
      in
      let rec eliminateArgs binds = function
        | Aster  t     -> `Aster (eliminateArgs binds t)
        | Plus   t     -> `Plus  (eliminateArgs binds t)
        | Opt    t     -> `Opt   (eliminateArgs binds t)
        | Alter  tl    -> `Alter (map (eliminateArgs binds) tl)
        | Juxt   tl    -> `Juxt  (map (eliminateArgs binds) tl)
        | Arg   (s, t) -> eliminateArgs ((checkName s) :: binds) t
        | Test  (s, f) -> `Test (s, f, binds)
        | EOS          -> `EOS
        | BOS          -> `BOS
      in
      let rec simplify = function
        | `Opt   t -> (match simplify t with `Opt t -> `Opt t | `Aster t -> `Aster t | `Plus t -> `Aster t | t -> `Opt t)
        | `Aster t -> (match simplify t with `Aster t | `Plus t | `Opt t | t -> `Aster t)
        | `Plus  t -> (match simplify t with `Plus t -> `Plus t | `Aster t | `Opt t -> `Aster t | t -> `Plus t)
        | `Juxt tl -> 
           (match
              flatten (
                map 
                  (fun t -> 
                     match simplify t with
                     | `Juxt tl -> tl
                     | t       -> [t]
                  ) 
                  tl
              )
             with
             | [t] -> t
             | tl  -> `Juxt tl
           )
        | `Alter tl -> 
           (match
              flatten (
                map 
                  (fun t -> 
                     match simplify t with
                     | `Alter tl -> tl
                     | t        -> [t]
                  ) 
                  tl
              )
            with
            | [t] -> t
            | tl  ->
               let opt, tl =
                 fold_left 
                   (fun (opt, tl) t ->
                      match t with
                      | `Aster t                -> opt && true, (`Plus t) :: tl
                      | `Juxt ((`Aster t) :: tl') -> 
                          let tl' = match tl' with [t] -> t | _ -> `Juxt tl' in
                          opt, tl' :: (`Juxt [`Plus t; tl'] :: tl)
                      | t -> opt, t :: tl
                   )  
                   (false, [])
                   tl
               in
               let t = `Alter tl in
               if opt then `Opt t else t
           )
        | `EOS    -> `EOS
        | `BOS    -> `BOS
        | `Test x -> `Test x
      in
      let id =
        let i = ref 0 in
        fun () -> 
          let j = ! i in
          incr i;
          j
      in
      let addElse branch node = setTrans ((Else, [], branch) :: (getTrans node)) node in      
      let rec inner succ = function
        | `Aster t -> 
	   let back = ref None in
           let t'   = inner (Back back, id ()) t in
	   back := Some t';
	   addElse succ t'

        | `Test (s, t, bs) -> State [If (s, t), bs, succ], (id ())
        | `Plus  t     -> inner succ (`Juxt [t; `Aster t]) 
        | `Opt   t     -> addElse succ (inner succ t)
        | `Alter tl    -> 
	    let tl = 
              map (fun t -> 
                match inner succ t with
                | ((Ok, _) as t) -> [Else, [], t]
                | t -> getTrans t
              ) 
              tl 
            in            
            State (flatten tl), (id ())  
        | `Juxt  tl    -> fold_right (fun t succ -> inner succ t) tl succ 
        | `BOS         -> State [BoS, [], succ], (id ())
        | `EOS         -> State [EoS, [], succ], (id ())      
      in  
      try `Ok (inner (Ok, id ()) (simplify (eliminateArgs [] expr))) with Duplicate name -> `Duplicate name

  end

