type regexp = 
  | Seq of regexp * regexp 
  | Star of regexp
  | Lower of string
  | Upper of string

let rec dump = function
  | Seq (re1, re2) -> "Seq(" ^ dump re1 ^ ", " ^ dump re2 ^ ")" 
  | Star re -> "Star(" ^ dump re ^ ")" 
  | Lower ch -> "Lower(" ^ ch ^ ")" 
  | Upper ch -> "Upper(" ^ ch ^ ")" 

let rec equal = function
  | Seq (re11, re12), Seq (re21, re22) -> equal (re11, re21) && equal (re12, re22)
  | Star re1, Star re2 -> equal (re1, re2)
  | Lower c1, Lower c2 -> c1 == c2
  | Upper c1, Upper c2 -> c1 == c2
  | _, _ -> false