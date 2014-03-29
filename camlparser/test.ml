open Ostap
open Combinators
open Printf

let _ =
   Combinators.debug := false;
   Combinators.lookahead := 5;
   let file = Sys.argv.(1) in
   let s = Util.read file in
   let _, stream = parse Parser.implementation (new Lexer.lexer s) in
   match stream#errors with
   | [] -> printf "parsed!\n"
   | e ->
      List.iter (fun e -> printf "%s\n" (Msg.toString (Matcher.Errors.toMsgFull e))) e;
      printf "%s\n" (Matcher.Errors.correct s e)
