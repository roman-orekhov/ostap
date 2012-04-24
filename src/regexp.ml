open Printf 
open List

type 'a t = 
    Test   of string * ('a -> bool)
  | Before of 'a t
  | Aster  of 'a t
  | Plus   of 'a t
  | Opt    of 'a t
  | Alter  of 'a t list
  | Juxt   of 'a t list
  | Bind   of string * 'a t
  | Arg    of string 
  | EOS

let rec fold f x e =
  let foldF = fold f in
  let x     = f x e in
  match e with
  | Before t | Aster t | Plus t | Opt t | Bind (_, t) -> foldF x t
  | Alter tl | Juxt tl -> fold_left (fun x t -> foldF x t) x tl 
  | _ -> x

let rec toText = 
  let ttl t   = Pretty.listByComma (List.map toText t) in
  let ptt n t = Pretty.plock (Pretty.string n) t in
  function
  | Test  (s, _) -> ptt "Test"    (Pretty.string s)
  | Before t     -> ptt "Before"  (toText t)
  | Aster  t     -> ptt "Aster"   (toText t)
  | Plus   t     -> ptt "Plus"    (toText t)
  | Opt    t     -> ptt "Opt"     (toText t)
  | Alter  tl    -> ptt "Alter"   (ttl    tl)
  | Juxt   tl    -> ptt "Juxt"    (ttl    tl)
  | Bind  (s, t) -> ptt "Bind"    (Pretty.seq  [Pretty.string s; toText t])
  | Arg    s     -> Pretty.string (sprintf "Arg (%s)" s)
  | EOS          -> Pretty.string "EOS"

let toString s = Pretty.toString (toText s)

module Diagram =
  struct

    type 'a expr = 'a t
    
    type 'a cond = If of string * ('a -> bool) | Ref of string | Lookahead of 'a t | Else | EoS 
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
        type 'a state   = 
           {
            epsilon   : int list; 
            eos       : int list; 
            symbol    : 'a -> 'a bnds -> (int * 'a bnds) list; 
            lookaheads: ('a t * int) list; 
            args      : (string * string list * int) list
           }
        and 'a t = {states : 'a state array; start: int; ok : int}

        let rec make (((root, start), _, num) : 'a diagram) = 
          let empty    = {epsilon = []; eos = []; symbol = (fun _ _ -> []); lookaheads = []; args = []} in
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
                 let epsilon, eos, trans, lkhds, args =
                   fold_left 
                     (fun (epsilon, eos, trans, lkhds, args) (cond, binds, dst) -> 
                        let dst    = derive dst    in
                        let dstId  = getId dst     in
		        let addDst = S.add (dstId) in
                        inner dst;
                        match cond with
                        | If (_, f)   -> epsilon, eos, (f, binds, dstId) :: trans, lkhds, args
                        | Ref       s -> epsilon, eos, trans, lkhds, (s, binds, dstId) :: args
                        | Lookahead t -> epsilon, eos, trans, (make t, dstId) :: lkhds, args 
                        | EoS         -> epsilon, addDst eos, trans, lkhds, args
                        | Else        -> addDst epsilon, eos, trans, lkhds, args
		     ) 
                     (S.empty, S.empty, [], [], [])
                     (getTrans x)
                 in
                 let trans a m =
                   flatten (map (fun (f, binds, dst) -> if f a then [dst, fold_left (fun m n -> bind n a m) m binds] else []) trans)
                 in
                 t.(id) <- {epsilon = elems epsilon; eos = elems eos; symbol = trans; lookaheads = lkhds; args = args}
            end
          in
          inner (root, start);
          {states = t; start = start; ok = !ok}
          
        let rec matchStream t s =
          let rec inner = function
            | (i, s, m) :: context ->
                LOG[traceNFA] (printf "state: %d\n" i);
                if i = t.ok 
                then (s, funOf m), context
                else 
                  let state    = t.states.(i) in                  
                  let context' =
                    (map       (fun i -> i, s, m) state.epsilon) @
                    (fold_left (fun acc (t, i) -> if Stream_ostap.endOf (matchStream t s) then acc else (i, s, m) :: acc) [] state.lookaheads
                    ) @
                    (fold_left 
                       (fun acc (arg, binds, i) -> 
                          let p     = funOf m arg in 
                          let s', n = Stream_ostap.eqPrefix p s in 
                          LOG[traceNFA] (
                            let module S = View.List (View.Char) in
                            printf "Matching argument: %s\n" arg;
                            printf "Value: %s\n" (S.toString (Obj.magic p));
                            printf "Stream: %s\n" (S.toString (Obj.magic (Stream_ostap.take 10 s)));
                            printf "Matched symbols: %d\n" n;
                            printf "Residual stream: %s\n" (S.toString (Obj.magic (Stream_ostap.take 10 s')))
                          );
                          if n = List.length p 
                          then 
                            let m' = List.fold_left (fun m name -> List.fold_right (fun x m -> bind name x m) p m) m binds in
			    (i, s', m') :: acc 
                          else acc
                       ) 
                       [] 
                       state.args
                    ) @ 
                    (try
                       let a, s' = Stream_ostap.get s in
                       map (fun (i, m) -> i, s', m) (state.symbol a m)
                     with End_of_file -> map (fun i -> i, s, m) state.eos
                    ) @ context                  
                  in
                  LOG[traceNFA] (
                    printf "next states: ";
                    List.iter (fun (i, _, _) -> printf "%d " i) context';
                    printf "\n"
                  );
                  inner context'              

            | [] -> raise End_of_file
          in
          Stream_ostap.fromIterator [t.start, s, empty] inner

      end

    let toDOT (r, _, _) =
      let clusterId =
        let counter = ref 0 in
        (fun () -> 
           let id = !counter in
           incr counter;
           id
        )
      in
      let rec toDOT prefix p =
        let module S = Set.Make (Compare.Integer) in
        let buf = Buffer.create 512 in
        let node id label = Buffer.add_string buf (sprintf "node_%s_%d [label=\"id=%d, %s\"];\n" prefix id id label) in 
        let edge id = 
          let doit t l =
            let inDOT i j label =
              Buffer.add_string buf (sprintf "node_%s_%d -> node_%s_%d [label=\"%s\"];\n" prefix i prefix j label)
            in
            inDOT id (getId (derive t)) l
          in
          let bindings =
            let module L = View.List (View.String) in
            L.toString
          in
          function
          | If (s, _)  , bs, t -> doit t (sprintf "if(%s)[%s]"  s (bindings bs))
          | Ref s      , bs, t -> doit t (sprintf "ref(%s)[%s]" s (bindings bs)) 
          | Else       , bs, t -> doit t (sprintf "else [%s]"     (bindings bs))
          | EoS        , _ , t -> doit t "EoS" 
          | Lookahead x, _ , t ->
              let r        = root x                     in
              let cId, rId = clusterId (), getId r      in
              let prefix'  = sprintf "%s_%d" prefix cId in
              Buffer.add_string buf (sprintf "subgraph cluster_%d {\n" cId);
              Buffer.add_string buf (sprintf "  label=\"lookahead\";\n");
              let str, okId = toDOT prefix' r in
              Buffer.add_string buf str;
              Buffer.add_string buf "}\n";
              Buffer.add_string buf (sprintf "node_%s_%d -> node_%s_%d;\n" prefix id prefix' rId);
              Buffer.add_string buf (sprintf "node_%s_%d -> node_%s_%d;\n" prefix' okId prefix (getId (derive t)))
        in
        let rec inner (sort, id) ((visited, ok) as context) =
          if S.mem id visited 
          then context
          else
            let (visited', _) as context' = S.add id visited, ok in
            match sort with
            | State trans -> 
                node id "state"; 
                fold_left (fun acc tran -> edge id tran; inner (getDest tran) acc) context' trans
           
            | Ok -> node id "ok"; (visited', Some id)

            | Back _ -> context'
        in
        let _, Some ok = inner p (S.empty, None) in
        Buffer.contents buf, ok
      in
      sprintf "digraph X {\n%s\n}\n" (fst (toDOT "" r))
    
    let rec make expr =
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
      let eliminateBindings expr = 
        let rec inner lookahead scoped binds = function
        | Aster  t     -> `Aster  (inner lookahead scoped binds t)
        | Before t     -> `Before  t
        | Plus   t     -> `Plus   (inner lookahead scoped binds t)
        | Opt    t     -> `Opt    (inner lookahead scoped binds t)
        | Alter  tl    -> `Alter  (map (inner lookahead scoped binds) tl)
        | Juxt   tl    -> `Juxt   (map (inner lookahead scoped binds) tl)
        | Bind  (s, t) -> inner lookahead (s :: scoped) ((checkName s) :: binds) t

        | Arg s -> 
           begin try 
             ignore (find (fun x -> s = x) scoped); 
             raise (Failure (sprintf "binding \"%s\" is used within capturing expression" s)) 
           with Not_found -> `Arg (s, binds) end

        | Test  (s, f) -> `Test (s, f, binds)
        | EOS          -> `EOS
        in
        inner false [] [] expr
      in
      let rec simplify = function
        | `Opt    t -> (match simplify t with `Opt t -> `Opt t | `Aster t -> `Aster t | `Plus t -> `Aster t | t -> `Opt t)
        | `Aster  t -> (match simplify t with `Aster t | `Plus t | `Opt t | t -> `Aster t)
        | `Plus   t -> (match simplify t with `Plus t -> `Plus t | `Aster t | `Opt t -> `Aster t | t -> `Plus t)
        | `Before t -> `Before t
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
        | `Arg  (s, bs)    -> return (State [Ref s, bs, succ], (id ()))

        | `Plus   t  -> return (inner succ (`Juxt [t; `Aster t]))
        | `Opt    t  -> return (addElse succ (inner succ t))
        | `Alter  tl -> return (State (flatten (map (fun t -> getTrans (inner succ t)) tl )), (id ()))
        | `Juxt   tl -> return (fold_right (fun t succ -> inner succ t) tl succ)
        | `Before t  -> return (State [Lookahead (make t), [], succ], (id ()))
        | `EOS       -> return (State [EoS, [], succ], (id ()))
      in        
      let d = inner (Ok, id ()) (simplify (eliminateBindings expr)) in
      (d, getBindings (), id ())      

  end

module ASCII =
  struct

    type expr = ASCII.t t

    let getOf a =
      let module M = Map.Make (struct type t = char include Pervasives end) in
      let m = 
        Array.fold_left 
          (fun m (c, x) ->
             if M.mem c m 
             then invalid_arg (sprintf "Ostap.Regexp.ASCII: internal error in initialization: duplicate character class '%c'" c)
             else M.add c x m
          )
          M.empty
          a
      in
      (fun c -> try Some (M.find c m) with Not_found -> None) 

    let s (Test (s, _) as x) = s.[0], x 

    open ASCII

    let ofChar  c x = x = c 
    let ofClass c x = Class.isIn c (Class.get x)
    
    let getClass = 
      getOf [|        
(*
        '$', Alter [EOS; Before (Test ("n", fun c -> c = '\n'))];
        '%', EOS;
*)
        '.', (fun c -> c != '\n');
        'n', (ofChar '\n');
        'r', (ofChar '\r');
        't', (ofChar '\t');
        'a', (ofChar '\x07');
        'e', (ofChar '\x1B');
        'f', (ofChar '\x0C');
        'v', (ofChar '\x0B');

        'P', (ofClass Class._PRINTABLE );
        'C', (ofClass Class._CONTROL   );
        'E', (ofClass Class._EXTENDED  );
        'O', (ofClass Class._OTHER     );

        'u', (ofClass Class._ULETTER   );
        'l', (ofClass Class._LLETTER   );
        'd', (ofClass Class._DDIGIT    );
        'w', (ofClass Class._WORD      );
        'b', (ofClass Class._BDIGIT    );
        'o', (ofClass Class._ODIGIT    );
        'x', (ofClass Class._HDIGIT    );
        'p', (ofClass Class._PUNCTUATOR);

        'B', (ofClass Class._BRACKET   );
        'H', (ofClass Class._LBRACKET  );
        'K', (ofClass Class._RBRACKET  );
        'A', (ofClass Class._ARITHMETIC);
        'R', (ofClass Class._RELATION  );
        'L', (ofClass Class._LOGIC     );
        'Q', (ofClass Class._QUOTE     )
      |]
          
    let range    = ASCII.range
    let nonrange = ASCII.nonrange
    let oneOf    = ASCII.oneOf

    exception Reason of int * string
(*
    let make s =
      let explain comment f s =
        let (_, pos), _ = Stream.get s in
        try f s with End_of_file -> raise (Reason (pos, comment))
      in
      let pos  s =
        let (_, pos), _ = Stream.get s in
        pos
      in
      let next s = 
        let (c, pos), s' = Stream.get s in
        c, s'
      in
      let rec ground s =
        let makeExpr c =
          match getClass c with
	  | Some t -> `T t
          | None   -> `C c
        in        
        explain 
           "unterminated escape sequence or interval" 
           (fun s ->
              match next s with
              | '\\', s' -> let c, s'' = next s' in makeExpr c, s''
              | '[' , s' ->
                 let rec inner acc s =
                   let t, s' = interval s in
                   let acc x = (acc x) or (t x) in
                   match next s' with
                   | ']', s'' -> acc, s''
                   | _        -> inner acc s'
                 in
                 let t, s'' = inner (fun _ -> false) s' in
                 (`T t), s''
                 
              |  c  , s' -> makeExpr c, s'
           )
           s
      and interval s =
        let charOf s = function
        | `T e -> raise (Reason (pos s, "character class in range expression"))
        | `C c -> c          
        in
        let e, s' = ground s in
	match next s' with
        | '-', s'' -> 
	   let c        = charOf s e in
	   let e', s''' = ground s'' in
           range c (charOf s'' e'), s''' 
          
        | _ -> (match e with `T e -> e | `C c -> ofChar c), s'
      and primary s =
        match next s with
        | '^', s' ->
        | '.', s' ->
        | '%', s' ->
        | '$', s' ->
        | _        -> 
           let t, s' = ground s in
           Test ("ground", t), s'
        
      
      let _ = Stream.zip (Stream.fromString s) (Stream.from 1) in
      ()
           
*)       

  end

let matchAll expr str =
  Diagram.Compiled.matchStream (Diagram.Compiled.make (Diagram.make expr)) str

let matchAllStr expr str = 
  let module S = View.ListC (struct let concat = (^) end) (View.Char) in
  Stream_ostap.map (fun (s, args) -> s, (fun name -> S.toString (args name))) (matchAll expr str)

