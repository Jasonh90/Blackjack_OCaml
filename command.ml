type command = 
  | Hit
  | Check
  | Bet of int
  | Quit

exception Malformed


(** [parse str] parses the terminal input into a command type. 
    Raise [Malformed] if command is not recognized*)
let parse str = match Str.split (Str.regexp " ") (String.lowercase_ascii str) with 
  | ["hit"] -> Hit
  | ["check"] -> Check
  | ["quit"] -> Quit
  | ["bet";n] -> (try Bet (int_of_string n) with Failure _ -> raise Malformed)
  | _ -> raise Malformed