type command = 
  | Hit
  | Check
  | Quit

exception Malformed


(** [parse str] parses the terminal input into a command type. 
    Raise [Malformed] if command is not recognized*)
let parse = function 
  | "hit" -> Hit
  | "check" -> Check
  | "quit" -> Quit
  | _ -> raise Malformed