open Printf 

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

    exception Duplicate of string

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
      let checkName, getArgs =
        let module S = Set.Make (String) in 
        let names    = ref S.empty in
        (fun name -> 
          if S.mem name !names then raise (Duplicate name) else names := S.add name !names; name       
        ),
        (fun () ->
          S.fold (fun x l -> x :: l) !names []
        )
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
        | `BOS         -> return (State [BoS, [], succ], (id ()))
        | `EOS         -> return (State [EoS, [], succ], (id ()))
      in  
      try 
        let d = inner (Ok, id ()) (simplify (eliminateArgs [] expr)) in
        `Ok (d, getArgs (), id ()) 
      with 
        Duplicate name -> `Duplicate name

  end

let matchAll expr str =
  match Diagram.make expr with
  | `Ok d           -> Diagram.Compiled.matchStream (Diagram.Compiled.make d) str
  | `Duplicate name -> invalid_arg (sprintf "duplicate argument name '%s' in regular expressioin" name)

let matchAllStr expr str = 
  let module S = View.ListC (struct let concat = (^) end) (View.Char) in
  Stream.map (fun (s, args) -> s, (fun name -> S.toString (args name))) (matchAll expr str)

