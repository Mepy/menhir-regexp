open Menhir_regexp.ParserInterface 
open Menhir_regexp.Syntax

let rec repl () = 
  print_string "> ";
  let input = read_line () in 
  print_endline ("parse: " ^ input);
  let regexp = parse input in 
  print_endline ("dump: " ^ dump regexp);
  print_endline ("print: " ^ print regexp);
  repl ()

let _ = repl ()