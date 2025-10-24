open Menhir_regexp.Syntax
open Menhir_regexp.ParserInterface

let eq_with_parse (s : string) (re_expect : regexp) =
  let re_parse = parse s in
  if not (equal (re_parse, re_expect)) then
    failwith ("parse \"" ^ s ^ "\" = " ^ (dump re_parse) ^ " != " ^ (dump re_expect))

let eq_with_print (re : regexp) (s_expect : string) = 
  let s_print = print re in 
  if not (s_print = s_expect) then 
    failwith ("print (" ^ dump re ^ ") = " ^ s_print ^ " != " ^ s_expect)

let test_left_assoc_of_seq () = 
  eq_with_parse "abc" (Seq (Seq (Lower "a", Lower "b"), Lower "c"))

let test_print () = 
  eq_with_print (Star (Lower "a")) "a*";
  eq_with_print (Star (Seq (Lower "a", Lower "b"))) "(ab)*";
  eq_with_print (Seq (Seq (Lower "a", Lower "b"), Lower "c" )) "abc";
  eq_with_print (Seq (Lower "a", Seq (Lower "b", Lower "c"))) "a(bc)";
  eq_with_print (Seq (Seq (Lower "a", Star (Lower "b")), Lower "c")) "ab*c"


let _ = test_print ()
let _ = test_left_assoc_of_seq ()