open Printf
open Msg

module MC = Map.Make(Coord)

type r = (int * (string * Coord.t)) list MC.t

let debug  = ref false

let line_regexp = Str.regexp "\r?\n#line \"\([^\"]*\)\" (\([0-9]+\):\([0-9]+\))\r?\n"

let stripLines s =
  let makeInt i = int_of_string (Str.matched_group i s) in
  (* pos, loc - position & location in resulting string,
     from - position in input string (after the last line directive) *)
  let rec inner from pos loc m acc =
     try
        if !debug then printf "loc was: %s\n" (Coord.toString loc);
        let first = Str.search_forward line_regexp s from in
        let len = first - from in
        let newpos = pos + len
        and loc = if first > from then Coord.shift loc s from first else loc
        and reloc = (Str.matched_group 1 s, (makeInt 2, makeInt 3))
        and current = try MC.find loc m with Not_found -> []
        and last = Str.match_end () in
        Buffer.add_substring acc s from len;
        if !debug then begin
           printf "loc is: %s\n" (Coord.toString loc);
           printf "'";
           for i = 0 to min 20 (String.length s - 1 - from) do printf "%c" s.[i+from] done;
           printf "'\n";
        end;
        inner last newpos loc (MC.add loc ((newpos, reloc)::current) m) acc
     with Not_found -> Buffer.add_substring acc s from (String.length s-from); m, Buffer.contents acc
  in inner 0 0 (1, 1) MC.empty (Buffer.create 1024)

let addFirst m = MC.add (0, 0) [0, ("", (0, 0))] m

let toString m =
  let module VL = View.List (View.Pair(View.Integer)(View.Pair(View.String)(Coord))) in
  let module M = View.Map(MC)(Coord)(VL) in
  M.toString m

let splitSucc c m =
  let prev, this, succ = MC.split c m in
  let (key, bnd) as res =
     match this with
     | Some item -> c, item
     | None -> MC.max_binding prev
  in
  res, MC.add key bnd succ

let shift s i loc_from loc_to reloc =
  let rec inner i loc reloc =
     if Coord.compare loc loc_to = 0
     then reloc
     else let next = Coord.next (s.[i] = '\n') in
          inner (i+1) (next loc) (next reloc)
  in inner i loc_from reloc

let getSuccReloc s m p =
  let (loc, relocs), succ = splitSucc p m in
  let (pos, (fil, reloc)) = List.hd relocs in
  let reloc = shift s pos loc p reloc in
  succ, (fil, reloc)

open Locator
let printReloc s m (p, q) =
  let succ, x = getSuccReloc s m p in
  let _, y = getSuccReloc s succ q in
  printf "%s -> %s\n" (toString (Interval (("", p), ("", q)))) (toString (Interval (x, y)))

let toLineDir loc s =
  let lfil, ll = least loc
  and rfil, rl = most loc in
  sprintf "\n#line \"%s\" %s\n%s\n#line \"%s\" %s\n" lfil (Coord.toString ll) s rfil (Coord.toString rl)



