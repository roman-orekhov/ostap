open Printf 
open List

type 'a t = 
    Test   of string * ('a -> bool)
  | Not    of 'a t
  | Before of 'a t
  | After  of 'a t
  | Aster  of 'a t
  | Plus   of 'a t
  | Opt    of 'a t
  | Alter  of 'a t list
  | Juxt   of 'a t list
  | Bind   of string * 'a t
  | Arg    of string 
  | BOS
  | EOS

let rec fold f x e =
  let foldF = fold f in
  let x     = f x e in
  match e with
  | Before t | After t | Aster t | Plus t | Opt t | Bind (_, t) | Not t -> foldF x t
  | Alter tl | Juxt tl -> fold_left (fun x t -> foldF x t) x tl 
  | _ -> x

let rec toText = 
  let ttl t   = Pretty.listByComma (List.map toText t) in
  let ptt n t = Pretty.plock (Pretty.string n) t in
  function
  | Test  (s, _) -> ptt "Test"    (Pretty.string s)
  | Not    t     -> ptt "Not"     (toText t)
  | Before t     -> ptt "Before"  (toText t)
  | After  t     -> ptt "After"   (toText t)
  | Aster  t     -> ptt "Aster"   (toText t)
  | Plus   t     -> ptt "Plus"    (toText t)
  | Opt    t     -> ptt "Opt"     (toText t)
  | Alter  tl    -> ptt "Alter"   (ttl    tl)
  | Juxt   tl    -> ptt "Juxt"    (ttl    tl)
  | Bind  (s, t) -> ptt "Bind"    (Pretty.seq  [Pretty.string s; toText t])
  | Arg    s     -> Pretty.string (sprintf "Arg (%s)" s)
  | BOS          -> Pretty.string "BOS"
  | EOS          -> Pretty.string "EOS"

let toString s = Pretty.toString (toText s)

module Diagram =
  struct

    type 'a expr = 'a t
    
    type 'a cond = If of string * ('a -> bool) | Else | EoS | BoS
    and  'a tran = 'a cond * string list * 'a node
    and  'a sort = State of 'a tran list | Ok | Back of 'a node option ref
    and  'a node = 'a sort * int
    and  'a t    = 'a node * string list * int

    let nnodes (_, _, n) = n
    let args   (_, n, _) = n
    let root   (n, _, _) = n

    let rec derive = function
      | Back r, _ -> let Some t = !r in derive t
      | t -> t

    let getId             = snd 
    let getDest (_, _, x) = x
    let getTrans          = function (State x, _) -> x | _ -> [] 
    let setTrans y        = function (State x, i) -> State y, i | _ -> invalid_arg "Ostap.Regexp.Diagram.setTrans" 

    module Compiled =
      struct

        module M = Map.Make (Compare.String)

        let empty       = M.empty
        let funOf m     = (fun name -> try rev (M.find name m) with Not_found -> [])
        let bind  n x m = try M.add n (x :: (M.find n m)) m with Not_found -> M.add n [x] m

        type 'a bnds    = 'a list M.t
        type 'a diagram = 'a t
        type 'a state   = {epsilon: int list; bos: int list; eos: int list; trans: ('a -> 'a bnds -> (int * 'a bnds) list)} 
        type 'a t       = {states: 'a state array; start: int; ok : int}

        let make (((root, start), _, num) : 'a diagram) = 
          let empty    = {epsilon = []; bos = []; eos = []; trans = (fun _ _ -> [])} in
          let ok       = ref 0 in
          let t        = Array.init num (fun _ -> empty) in
          let filled   = Array.make num false            in
          let module S = Set.Make (Compare.Integer)      in
          let elems  s = S.fold (fun x l -> x :: l) s [] in
          let rec inner ((node, id) as x) =
            if not filled.(id) then 
            begin
              filled.(id) <- true;
              match node with
              | Ok -> ok := id
              | _  ->
                 let epsilon, bos, eos, trans =
                   fold_left 
                     (fun (epsilon, bos, eos, trans) (cond, binds, dst) -> 
                        let dst    = derive dst    in
                        let dstId  = getId dst     in
		        let addDst = S.add (dstId) in
                        inner dst;
                        match cond with
                        | If (_, f) -> epsilon, bos, eos, (f, binds, dstId) :: trans
                        | EoS       -> epsilon, bos, addDst eos, trans
                        | BoS       -> epsilon, addDst bos, eos, trans
                        | Else      -> addDst epsilon, bos, eos, trans
		     ) 
                     (S.empty, S.empty, S.empty, [])
                     (getTrans x)
                 in
                 let trans a m =
                   flatten (map (fun (f, binds, dst) -> if f a then [dst, fold_left (fun m n -> bind n a m) m binds] else []) trans)
                 in
                 t.(id) <- {epsilon = elems epsilon; bos = elems bos; eos = elems eos; trans = trans}
            end
          in
          inner (root, start);
          {states = t; start = start; ok = !ok}
          
        let matchStream t s =
          let rec inner = function
            | (i, s, bos, m) :: context ->
                LOG[traceNFA] (printf "state: %d\n" i);
                if i = t.ok 
                then (s, funOf m), context
                else 
                  let state    = t.states.(i) in                  
                  let context' =
                    (map (fun i -> i, s, bos, m) ((if bos then state.bos else []) @ state.epsilon)) @
                    (try
                       let a, s' = Stream.get s in
                       map (fun (i, m) -> i, s', false, m) (state.trans a m)
                     with End_of_file -> map (fun i -> i, s, bos, m) state.eos
                    ) @ context                  
                  in
                  LOG[traceNFA] (
                    printf "next states: ";
                    List.iter (fun (i, _, _, _) -> printf "%d " i) context';
                    printf "\n"
                  );
                  inner context'              

            | [] -> raise End_of_file
          in
          Stream.fromIterator [t.start, s, true, empty] inner

      end

    let toDOT (root, _, _) =
      let buf = Buffer.create 512 in
      Buffer.add_string buf "digraph X {\n";
      let node id label = Buffer.add_string buf (sprintf "node%d [label=\"id=%d, %s\"];\n" id id label) in      
      let edge id = 
        let doit t l =
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
      let checkName, getBindings =
        let module S = Set.Make (String) in 
        let names    = ref S.empty in
        (fun name -> 
          names := S.add name !names; 
          name
        ),
        (fun () ->
          S.fold (fun x l -> x :: l) !names []
        )
      in      
      let getLookBehindLength expr =
        let bind f x y =
          match x, y with
          | `None , x | x, `None  -> x
          | `Unlim, _ | _, `Unlim -> `Unlim 
          | `Lim x, `Lim y -> `Lim (f x y)
        in
        let add = bind (+) in
        let max = bind max in
        let rec capturedLength s = 
          let module M = Map.Make (Compare.String) in
          let m = 
            fold 
              (fun m t -> 
                 match t with 
                 | Bind (s, t) -> 
                    let l = length t in
                    (try M.add s (max (M.find s m) l) m with Not_found -> M.add s l m)

                 | _ -> m
              ) 
              M.empty 
              expr 
          in
          (try M.find s m with Not_found -> `None)
        and length = function
          | Test _           -> `Lim 1
          | BOS     | EOS    -> `Lim 0
          | Aster _ | Plus _ -> `Unlim

          | Not t | Bind (_, t) | Before t | After t | Opt t -> length t

          | Alter tl -> fold_left (fun x t -> max x (length t)) `None tl
          | Juxt  tl -> fold_left (fun x t -> add x (length t)) `None tl

          | Arg s -> capturedLength s
        in
        fold
          (fun acc e ->
             match e with 
             | After t -> max acc (length t)
             | _       -> acc
          )
         `None
          expr
      in
      let eliminateBindings expr = 
        let rec inner scoped binds = function
        | Aster  t     -> `Aster  (inner scoped binds t)
        | Not    t     -> `Not    (inner scoped binds t)
        | Before t     -> `Before (inner scoped binds t)
        | After  t     -> `After  (inner scoped binds t)
        | Plus   t     -> `Plus   (inner scoped binds t)
        | Opt    t     -> `Opt    (inner scoped binds t)
        | Alter  tl    -> `Alter  (map (inner scoped binds) tl)
        | Juxt   tl    -> `Juxt   (map (inner scoped binds) tl)
        | Bind  (s, t) -> inner (s :: scoped) ((checkName s) :: binds) t

        | Arg s -> 
           begin try 
             ignore (find (fun x -> s = x) scoped); 
             raise (Failure (sprintf "binding \"%s\" is used within capturing expression" s)) 
           with Not_found -> `Arg s end

        | Test  (s, f) -> `Test (s, f, binds)
        | EOS          -> `EOS
        | BOS          -> `BOS
        in
        inner [] [] expr
      in
      let rec simplify = function
        | `Opt    t -> (match simplify t with `Opt t -> `Opt t | `Aster t -> `Aster t | `Plus t -> `Aster t | t -> `Opt t)
        | `Aster  t -> (match simplify t with `Aster t | `Plus t | `Opt t | t -> `Aster t)
        | `Plus   t -> (match simplify t with `Plus t -> `Plus t | `Aster t | `Opt t -> `Aster t | t -> `Plus t)
        | `Not    t -> (match simplify t with `Not  t -> t | t -> `Not t)
        | `Before t -> `Before t
        | `After  t -> `After  t
        | `Juxt   tl -> 
           (match
              flatten (
                map 
                  (fun t -> 
                     match simplify t with
                     | `Juxt tl -> tl
                     | t        -> [t]
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
                     | t         -> [t]
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
                      | `Aster t -> opt && true, (`Plus t) :: tl
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
        | `Arg s  -> `Arg s
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
      let registerNode, busyNode =
        let module S = Set.Make (struct type t = int include Pervasives end) in
        let ids = ref S.empty in
        (fun n -> ids := S.add (getId n) !ids),
        (fun (tag, id) -> tag = Ok || (S.mem id !ids))
      in
      let addElse branch node = 
        setTrans ((if busyNode branch then [Else, [], branch] else getTrans branch) @ (getTrans node)) node 
      in
      let rec inner succ = 
        let return y = registerNode succ; y in
        function
        | `Aster t -> 
	   let back = ref None in
           let t'   = inner (Back back, id ()) t in
           registerNode t';
	   back := Some t';
	   return (addElse succ t')

        | `Test (s, t, bs) -> return (State [If (s, t), bs, succ], (id ()))
        | `Plus  t     -> return (inner succ (`Juxt [t; `Aster t]))
        | `Opt   t     -> return (addElse succ (inner succ t))
        | `Alter tl    -> return (State (flatten (map (fun t -> getTrans (inner succ t)) tl )), (id ()))
        | `Juxt  tl    -> return (fold_right (fun t succ -> inner succ t) tl succ)

        | `Not    _
        | `Arg    _
        | `Before _ 
        | `After  _    -> invalid_arg "not supported"

        | `BOS         -> return (State [BoS, [], succ], (id ()))
        | `EOS         -> return (State [EoS, [], succ], (id ()))
      in        
      let d = inner (Ok, id ()) (simplify (eliminateBindings expr)) in
      (d, getBindings (), id ())      

  end

(*
module ASCII =
  struct

    type expr = ASCII.t t

    let getOf a =
      let module M = Map.Make (struct type t = char include Pervasives end) in
      let m = 
        Array.fold_left 
          (fun m (c, x) ->
             if M.find c m 
             then invalid_arg (sprintf "Ostap.Regexp.ASCII: internal error in initialization: duplicate character class '%c'" c)
             else M.add c x m
          )
          M.empty
          a
      in
      (fun c -> try Some (M.find c m) with Not_found -> None) 

    let s (Test (s, _) as x) = s.[0], x 

    let getSpecial = 
      getOf [|
        s Test (".", fun c -> c != '\n');
        '$', Alter [EOS; Before (Test ("n", fun c -> c = '\n'))]; 
        '^', Alter [BOS; After  (Test ("n", fun c -> c = '\n'))];
        '&', BOS;
        '%', EOS;
        
      |]

    let getClass = 
      getOf [|
        s Test ("n", ofChar '\n');
        s Test ("r", ofChar '\r');
        s Test ("t", ofChar '\t');
        s Test ("a", ofChar '\x07');
        s Test ("e", ofChar '\x1B');
        s Test ("f", ofChar '\x0C');
        s Test ("v", ofChar '\x0B');

        s Test ("P", ofClass ASCII.Class._PRINTABLE );
        s Test ("C", ofClass ASCII.Class._CONTROL   );
        s Test ("E", ofClass ASCII.Class._EXTENDED  );
        s Test ("O", ofClass ASCII.Class._OTHER     );

        s Test ("u", ofClass ASCII.Class._ULETTER   );
        s Test ("l", ofClass ASCII.Class._LLETTER   );
        s Test ("d", ofClass ASCII.Class._DDIGIT    );
        s Test ("w", ofClass ASCII.Class._WORD      );
        s Test ("b", ofClass ASCII.Class._BDIGIT    );
        s Test ("o", ofClass ASCII.Class._ODIGIT    );
        s Test ("x", ofClass ASCII.Class._HDIGIT    );
        s Test ("p", ofClass ASCII.Class._PUNCTUATOR);

        s Test ("B", ofClass ASCII.Class._BRACKET   );
        s Test ("H", ofClass ASCII.Class._LBRACKET  );
        s Test ("K", ofClass ASCII.Class._RBRACKET  );
        s Test ("A", ofClass ASCII.Class._ARITHMETIC);
        s Test ("R", ofClass ASCII.Class._RELATION  );
        s Test ("L", ofClass ASCII.Class._LOGIC     );
        s Test ("Q", ofClass ASCII.Class._QUOTE     )
      |]
          
    let range    = ASCII.range
    let nonrange = ASCII.nonrange
    let oneOf    = ASCII.oneOf
      
    let make s = 
      let next s =
        let rec inner f s =
          let (x, i), s' = Stream.get s in          
          (match x with
          | '('  -> `RBR, s'
          | ')'  -> `RCT, s'
          | '['  -> `SBR, s'
          | ']'  -> `SCT, s'
          | '{'  -> `CBR, s'
          | '}'  -> `CCT, s'
          | ':'  -> `CLN, s'
          | '|'  -> `BAR, s'
          | '-'  -> `DSH, s'
          | '~'  -> `TLD, s'
          | '*'  -> `AST, s'
          | '+'  -> `STR, s'
          | '.'  -> `DOT, s'
          | '?'  -> `QTN, s'
          | '^'  -> `BEG, s'
          | '$'  -> `END, s'
          | '%'  -> `BOS, s'
          | '&'  -> `EOS, s'
          | '\\' -> (if f then `CHR '\\' else `ESC (inner true s')), s'
          | _    -> `CHR x, s'
          ), i
        in
        try inner false s with End_of_file -> `FIN
      in      
      let symclass s =
        let rec inner s =
          let n, s' = next s in
          if 
        in
      in
      let rec parse acc s =
        if Stream.endOf s 
        then acc
        else 
          let 
      in
      parse [] (Stream.zip (Stream.fromString s) (Stream.from 0))

  end
*)

let matchAll expr str =
  Diagram.Compiled.matchStream (Diagram.Compiled.make (Diagram.make expr)) str

let matchAllStr expr str = 
  let module S = View.ListC (struct let concat = (^) end) (View.Char) in
  Stream.map (fun (s, args) -> s, (fun name -> S.toString (args name))) (matchAll expr str)

