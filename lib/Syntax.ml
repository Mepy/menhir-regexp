type regexp = 
  | Or of regexp * regexp (* 1 *)
  | Seq of regexp * regexp (* 2 *)
  | Star of regexp (* 3 *)
  | Lower of string (* 4 *)
  | Upper of string (* 4 *)


let print re = 
  let rec go prio = function
  | Or (re1, re2) ->
    let inner = go 1 re1 ^ "|" ^ go 1 re2 in
    if 1 > prio then inner else "(" ^ inner ^ ")"
  | Seq (re1, re2) -> 
    let inner = go 2 re1 ^ go 2 re2 in
    if 1 > prio then inner else "(" ^ inner ^ ")"
  | Star re -> 
    let inner = go 3 re ^ "*" in 
    if 3 > prio then inner else "(" ^ inner ^ ")"
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