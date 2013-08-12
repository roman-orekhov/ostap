open Printf

module type StreamChar =
   sig
      type t
      val compare : t -> t -> int
      val values  : t list
      val toInt   : t -> int
      val ofInt   : int -> t
      val toString: t -> string
      val max     : int
   end

module ASCIIStreamChar =
   struct
      type t = char

      let compare = compare

      let values = Ostream.take 256 (Ostream.fromIterator 0 (fun i -> char_of_int i, i+1))

      let toInt = int_of_char
      let ofInt = char_of_int
      let toString = function
         | '"' -> "\\\""
         | c -> if ASCII.Class.isIn ASCII.Class._PRINTABLE (ASCII.Class.get c)
                then sprintf "%c" c
                else sprintf "\%d" (toInt c)
      let max = List.length values
   end

module DetNFA (C : StreamChar) =
   struct
      open Regexp.Diagram
      module MI = Map.Make(Compare.Integer) (* to get old states from new states *)
      module M = Map.Make (Compare.String)
      module S = SS
      module SI = Set.Make (Compare.Integer)
      module MSI = Map.Make(SI) (* to get new states from old states *)

      type bnds  = C.t list M.t
      type state = 
         {
          eos       : int option; 
          lookaheads: (t * int) list; 
          args      : (int * S.t) M.t;
          symbols   : (int * S.t) array
         }
      and t = {states : state array; ok : int list} (* start always from 0 *)
      
      module MD = Map.Make(struct type a = t type t = a let compare = Pervasives.compare end)

      let rec make ((nodes, _, num) : C.t Regexp.Diagram.t) =
         let ok       = ref [] in
         let created  = ref 0 in
         let cur      = ref 0 in
         let oldToNew = ref MSI.empty in
         let newToOld = ref MI.empty in
         let table    = ref [] in
         let getIds lst = List.fold_left (fun set node -> SI.add node.id set) SI.empty lst in
         let module VN = struct type t = C.t Regexp.Diagram.node let toString = fun node -> string_of_int node.id end in
         let setId lst =
            let module VLN = View.List(VN) in
(*            printf "setting #%d to %s\n" !created (VLN.toString lst);*)
            newToOld := MI.add !created lst !newToOld;
            oldToNew := MSI.add (getIds lst) !created !oldToNew;
            created := !created + 1;
            !created - 1
         in
         ignore (setId nodes);
         while MI.mem !cur !newToOld do
            let oldStates = MI.find !cur !newToOld in (* Diagram.node list *)
            if List.exists (fun node -> node.final) oldStates then ok := !cur :: !ok;
            let trans, refs, eos, lookaheads =
               List.fold_left 
               (fun acc node ->
                  List.fold_left
                     (fun (trans, refs, eos, lookaheads) (cond, bnds, node) ->
                        match cond with
                        | If (_, f) ->
                           List.iter
                              (fun c ->
                                 if f c then
                                    let states, binds = trans.(C.toInt c) in
                                    trans.(C.toInt c) <- node::states, S.union binds bnds
                              )
                              C.values;
                           trans, refs, eos, lookaheads
                        | Ref arg ->
                           let states, binds = try M.find arg refs with Not_found -> [], S.empty in
                           trans, M.add arg (node::states, S.union binds bnds) refs, eos, lookaheads
                        | EoS -> trans, refs, node::eos, lookaheads
                        | Lookahead t ->
                           let t = make t in
                           let states = try MD.find t lookaheads with Not_found -> [] in
                           trans, refs, eos, MD.add t (node::states) lookaheads
                     )
                     acc
                     node.transitions
               )
               (Array.make C.max ([], S.empty), M.empty, [], MD.empty)
               oldStates
            in
            let toId states = try MSI.find (getIds states) !oldToNew with Not_found -> setId states in
            let update states, binds =
               (match states with
               | [] -> -1
               | _ -> toId states
               ), binds
            in
            table :=
               {symbols = Array.map update trans;
                args = M.map update refs;
                lookaheads = MD.fold (fun t states acc -> if states = [] then acc else (t, toId states)::acc) lookaheads [];
                eos = match eos with [] -> None | _ -> Some (toId eos)
               } :: !table;
            cur := !cur + 1
         done;
         {states = Array.of_list (List.rev !table); ok = !ok}

      let rec toDiagram reverse t =
         let l = Array.length t.states in
         let res = Array.init l (fun i -> {final = false; transitions = []; id = i}) in
         List.iter (fun i -> res.(i) <- {res.(i) with final = true}) (if reverse then [0] else t.ok);
         Array.iteri
            (fun beg_id state ->
               let updTrans end_id cond binds =
                  let b, e = if reverse then end_id, beg_id else beg_id, end_id in
                  res.(b).transitions <- (cond, binds, res.(e))::res.(b).transitions in
               if reverse
               then Array.iteri
                  (fun i (end_id, binds) ->
                     if end_id >= 0 then
                     updTrans end_id (If (sprintf "%s" (C.toString (C.ofInt i)), fun c -> c = C.ofInt i)) binds
                  )
                  state.symbols
               else begin
                  let module P = Compare.Pair(Compare.Integer)(S) in
                  let module MIS = Map.Make(P) in
                  let update arrow (a, b) acc =
                     if fst arrow >= 0
                     then
                        let (name, func) = try MIS.find arrow acc with Not_found -> ("", fun c -> false) in
                        let a, b = C.ofInt a, C.ofInt b in
                        let name = if name = "" then "" else sprintf "%s," name in
                        let newval =
                           if C.compare a b = 0
                           then name ^ C.toString a, fun c -> C.compare c a = 0 || func c
                           else sprintf "%s%s-%s" name (C.toString a) (C.toString b), fun c -> C.compare a c <= 0 || C.compare c b <= 0 || func c
                        in
                        MIS.add arrow newval acc
                     else acc
                  in
                  let _, last_arrow, (last_from, arrows) =
                     Array.fold_left
                        (fun (i, prev, (from, acc)) (end_id, _ as arrow) ->
                           i + 1,
                           arrow,
                           (if P.compare prev arrow != 0
                           then i, update prev (from, i - 1) acc
                           else from, acc)
                        )
                        (0, ((-2), S.empty), (0, MIS.empty))
                        state.symbols
                  in
                  let arrows = update last_arrow (last_from, C.max - 1) arrows in
                  MIS.iter
                     (fun (end_id, binds) (name, func) ->
                        updTrans end_id (If (name, func)) binds
                     )
                     arrows
               end;
               M.iter
                  (fun arg (end_id, binds) -> updTrans end_id (Ref arg) binds)
                  state.args;
               (match state.eos with None -> () | Some end_id -> updTrans end_id EoS S.empty);
               List.iter
                  (fun (t, end_id) -> updTrans end_id (Lookahead (toDiagram reverse t)) S.empty)
                  state.lookaheads
            )
            t.states;
         if reverse
         then (List.map (fun i -> res.(i)) t.ok), [], l
         else [res.(0)], [], l

      let toDOT t = Regexp.Diagram.toDOT (toDiagram false t)

      let printTable t =
         let lkhdnum = ref 0 in
         let lkhds = Hashtbl.create 10 in
         let getNum t = try Hashtbl.find lkhds t with Not_found -> Hashtbl.add lkhds t !lkhdnum; lkhdnum := !lkhdnum + 1; !lkhdnum - 1 in
         Array.iteri
         (fun i state ->
            printf "%d%c: " i (if List.mem i t.ok then 'f' else ' ');
            let module VSS = View.Set(S)(View.String) in
            Array.iteri (fun i id, binds -> if id >= 0 then printf "'%s' -> %d, %s " (C.toString (C.ofInt i)) id (VSS.toString binds)) state.symbols;
            M.iter (fun arg    id, binds -> if id >= 0 then printf "[%s] -> %d, %s " arg                      id (VSS.toString binds)) state.args;
            (match state.eos with None -> () | Some id ->   printf  "EoS -> %d "                              id);
            List.iter (fun t, id ->                         printf "#%d  -> %d "                   (getNum t) id)                      state.lookaheads;
            printf "\n"
         )
         t.states

      let minimize diag =
         let reverse = toDiagram true in
         make (reverse (let t = make (reverse (make diag)) in (*printTable t;*) t))

   end
