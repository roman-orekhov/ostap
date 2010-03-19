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
  let ws     = Test ("ws"    , fun c -> c = ' '                                         ) in
  let nows   = Test ("nows"  , fun c -> c != ' '                                        ) in

  let ident  = Juxt [Arg ("ID", Juxt [letter; Aster (Alter [letter; digit])]); Arg ("NEXT", Alter [noid; EOS])] in
  let decim  = Juxt [Arg ("DC", Plus digit); Arg ("NEXT", Alter [nodig; EOS])]                                  in
 
  let item   = Alter [ident; decim]                     in
  let wss    = Juxt  [Aster ws; Arg ("NEXT", Alter [nows; EOS])] in
   
  let item   = matchAllStr item in
  let wss    = matchAllStr wss  in

  let analyseString str =
    printf "Analysing string \"%s\"\n" str;
    let rec inner s =
      if Stream.endOf s then ()
      else 
        let m = item s in
        if Stream.endOf m then ()
        else begin          
          printf "matched:\n"; 
          print ["ID"; "DC"; "NEXT"] m;
          let (s', m) = Stream.hd m in
          let s'' = Stream.concat (Stream.fromString (m "NEXT")) s' in
          let m   = wss s'' in          
          inner (if Stream.endOf m then s'' else let (s', m) = Stream.hd m in Stream.concat (Stream.fromString (m "NEXT")) s')
        end
    in
    inner (Stream.fromString str);
    printf "End of analysis\n"
  in
  analyseString "123abc";
  analyseString "123 abc def 12 3 4 5 d d";
  analyseString "123 abc ddd ccc";
  analyseString "123";
  analyseString "abc"
;;