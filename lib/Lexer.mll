{
open Lexing
open Parser
}

let whitespace = [' ' '\t']+

let lower = ['a'-'z']
let upper = ['A'-'Z']
let star = '*'
let or = "|"
let lparen = "("
let rparen = ")"


rule token = parse 
    | lower { LOWER(Lexing.lexeme lexbuf) }
    | upper { UPPER(Lexing.lexeme lexbuf) }
    | star { STAR }
    | or { OR }
    | lparen { LPAREN }
    | rparen { RPAREN }
    | whitespace { token lexbuf }
    | eof { EOF }
    | _ { failwith "Unknown character" }