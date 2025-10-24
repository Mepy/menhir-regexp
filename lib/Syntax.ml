type regexp = 
  | Or of regexp * regexp (* 1 2 *)
  | Seq of regexp * regexp (* 3 4 *)
  | Star of regexp (* 5 6 *)
  | Lower of string (* 7 *)
  | Upper of string (* 7 *)

let rec dump = function 
  | Or (re1, re2) -> 
    "Or (" ^ dump re1 ^ ", " ^ dump re2 ^ ")" 
  | Seq (re1, re2) -> 
    "Seq (" ^ dump re1 ^ ", " ^ dump re2 ^ ")" 
  | Star re -> "Star (" ^ dump re ^ ")"
  | Lower ch -> "Lower \"" ^ ch ^ "\""
  | Upper ch -> "Upper \"" ^ ch ^ "\""

let print re = 
  let rec go prio = function
  | Or (re1, re2) ->
    let inner = go 1 re1 ^ "|" ^ go 2 re2 in
    if 1 >= prio then inner else "(" ^ inner ^ ")"
  | Seq (re1, re2) -> 
    let inner = go 3 re1 ^ go 4 re2 in
    if 3 >= prio then inner else "(" ^ inner ^ ")"
  | Star re -> 
    let inner = go 5 re ^ "*" in 
    if 5 >= prio then inner else "(" ^ inner ^ ")"
  | Lower ch -> ch
  | Upper ch -> ch
  in go 0 re 

let rec equal = function
  | Or (re11, re12), Or (re21, re22) -> equal (re11, re21) && equal (re12, re22)
  | Seq (re11, re12), Seq (re21, re22) -> equal (re11, re21) && equal (re12, re22)
  | Star re1, Star re2 -> equal (re1, re2)
  | Lower c1, Lower c2 -> c1 = c2
  | Upper c1, Upper c2 -> c1 = c2
  | _, _ -> false