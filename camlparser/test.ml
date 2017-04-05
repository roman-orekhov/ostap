open Ostap
open Combinators
open Printf

let _ =
   Combinators.debug := false;
   Combinators.lookahead := 5;
   let file = Sys.argv.(1) in
   let s = Util.read file in
   let pfun = if Filename.check_suffix file ".mli" then Parser.interface else Parser.implementation in
   let a = Sys.time () in
   let _, stream = parse pfun (new Lexer.lexer file s) in
   let b = Sys.time () in
   printf "time: %f\n" ((b -. a) *. float 1000);
   match stream#errors with
   | [] -> printf "parsed!\n"
   | e ->
      List.iter (fun e -> printf "%s\n" (Msg.toString (Matcher.Errors.toMsgFull stream#reloc e))) e;
      printf "%s\n" (Matcher.Errors.correct s e)
