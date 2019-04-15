type command = 
  | Hit
  | Check

exception Malformed


(** [parse str] parses the terminal input into a command type. 
    Raise [Malformed] if command is not recognized*)
let parse str = 
  if str = "hit" then Hit 
  else if str = "check" then Check
  else raise Malformed