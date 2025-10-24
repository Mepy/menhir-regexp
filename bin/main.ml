open Menhir_regexp.ParserInterface 
open Menhir_regexp.Syntax

let rec repl () = 
  print_string "> ";
  let input = read_line () in 
  print_endline input;
  let regexp = parse input in 
  print_string (print regexp);
  print_endline "";
  repl ()

let _ = repl ()