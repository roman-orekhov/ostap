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

    type 'a expr = 'a t
    
    type 'a cond = If of string * ('a -> bool) | Else | EoS | BoS
    and  'a tran = 'a cond * 'a t
    and  'a sort = State of 'a tran list | Ok | Back of 'a t option ref | Begin of string * 'a t | End of string * 'a t 
    and  'a t    = 'a sort * int

    exception Duplicate of string

    let getId      = snd 
    let getTrans   = function (State x, _) -> x | _ -> [] 
    let setTrans y = function (State x, i) -> State y, i | _ -> invalid_arg "Ostap.Regexp.Diagram.setTrans" 

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
        function
        | If (s, _), t -> doit t (sprintf "if(%s)" s)
        | Else     , t -> doit t "else"
        | EoS      , t -> doit t "EoS"
        | BoS      , t -> doit t "BoS"
      in
      let rec inner (sort, id) =
        match sort with
        | State trans -> 
            node id "state";
            List.iter (fun tran -> edge id tran; inner (snd tran)) trans
           
        | Ok -> node id "ok"

        | Back _ -> ()

        | Begin (name, t) -> 
            node id (sprintf "begin(%s)" name);
            edge id (Else, t);
            inner t

        | End (name, t) ->
            node id (sprintf "end(%s)" name);
            edge id (Else, t);
            inner t
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
      let id =
        let i = ref 0 in
        fun () -> 
          let j = ! i in
          incr i;
          j
      in
      let addElse branch node = setTrans ((Else, branch) :: (getTrans node)) node in      
      let rec inner succ = function
        | Aster t -> 
	   let back = ref None in
           let t'   = inner (Back back, id ()) t in
	   back := Some t';
	   addElse succ t'

        | Test (s, t) -> State [If (s, t), succ], (id ())
        | Plus  t     -> inner succ (Juxt [t; Aster t]) 
        | Opt   t     -> addElse succ (inner succ t)
        | Alter tl    -> State (List.flatten (List.map (fun t -> getTrans (inner succ t)) tl)), (id ())  
        | Juxt  tl    -> List.fold_right (fun t succ -> inner succ t) tl succ 
        | Arg (s, t)  -> Begin (checkName s, inner (End (s, succ), (id ())) t), (id ()) 
        | BOS         -> State [BoS, succ], (id ())
        | EOS         -> State [EoS, succ], (id ())      
      in  
      try `Ok (inner (Ok, id ()) expr) with Duplicate name -> `Duplicate name

  end

