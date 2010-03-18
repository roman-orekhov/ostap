open Ostap
open Regexp
open Printf

let _ =
  let module S = View.List (View.String) in
  let rest  s = sprintf "%s..." (Stream.takeStr 10 s) in 
  let print names s = 
    Stream.iter 
      (fun (s, b) -> 
         printf "  stream: %s;\n  args  : %s\n" 
           (rest s) 
           (S.toString (List.map (fun n -> sprintf "%s=[%s]" n (b n)) names))
      ) 
      s 
  in
  let letter = Test ("letter", fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) in
  let noid   = Test ("noid"  , fun c -> (c < 'a' || c > 'z') && (c < 'A' || c > 'Z') && (c < '0' || c > '9')) in
  let digit  = Test ("digit" , fun c -> c >= '0' && c <= '9'                            ) in
  let nodig  = Test ("nodig" , fun c -> c < '0' || c > '9'                              ) in
  let sign   = Test ("sign"  , fun c -> c = '+' || c = '-' || c = '*' || c = '/'        ) in
  let ws     = Test ("ws"    , fun c -> c = ' '                                         ) in

  let ident  = Juxt [Arg ("ID", Juxt [letter; Aster (Alter [letter; digit])]); Arg ("NEXT", Alter [noid; EOS])] in
  let decim  = Arg ("DC", Plus digit)                                                             in
  let sign   = Arg ("SG", sign)                                                                   in
  let wss    = Aster ws                                                                           in

  let root   = Juxt [Aster (Alter [ident; decim; sign]); EOS] in

 (*  printf "%s" (Diagram.toDOT (match Diagram.make ident with `Ok x ->  x | _ -> invalid_arg ""))  ; *)

  let m0     = matchAllStr ident in


  printf "matching ident against \"aaaa123+--+\":\n";
  print ["ID"; "NEXT"] (m0 (Stream.fromString "aaaa123+--+"));
  printf "matching ident against \"b+b-c+2*4\":\n";
  print ["ID"; "NEXT"] (m0 (Stream.fromString "b+b-c+2*4"))

;;