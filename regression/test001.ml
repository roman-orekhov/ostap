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
  let m0 = matchAllStr (Arg ("A", Aster (Test ("a", (fun c -> c = 'a'))))) in
  printf "matching a* against \"aaaa\":\n";
  print ["A"] (m0 (Stream.fromString "aaaa"));
  printf "matching a* against \"baaa\":\n";
  print ["A"] (m0 (Stream.fromString "baaa"));
  let m1 = matchAllStr (Arg ("A", Juxt [Aster (Test ("a", (fun c -> c = 'a'))); EOS])) in
  printf "matching a*$ against \"aaaa\":\n";
  print ["A"] (m1 (Stream.fromString "aaaa"));
  printf "matching a* against \"baaa\":\n";
  print ["A"] (m1 (Stream.fromString "baaa"))
;;