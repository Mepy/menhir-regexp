open Menhir_regexp.Syntax
open Menhir_regexp.ParserInterface

let eq_with_parse (s : string) (re : regexp) =
  if not (equal ((parse s), re)) then
    failwith ("parse \"" ^ s ^ "\" != " ^ (dump re))


let test_left_assoc_of_seq () = 
  eq_with_parse "abc" (Seq (Seq (Lower "a", Lower "b"), Lower "c"))


let _ = test_left_assoc_of_seq ()