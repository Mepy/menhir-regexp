{
open Lexing
open Parser
}

let whitespace = [' ' '\t']+

let lower = ['a'-'z']
let upper = ['A'-'Z']
let star = '*'


rule token = parse 
    | lower { LOWER(Lexing.lexeme lexbuf) }
    | upper { UPPER(Lexing.lexeme lexbuf) }
    | star { STAR }
    | whitespace { token lexbuf }
    | eof { EOF }
    | _ { failwith "Unknown character" }