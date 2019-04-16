(**
   Parsing of player commands.
*)

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command = 
  | Hit
  | Check
  | Quit

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses the terminal input into a command type. 
    Raise [Malformed] if command is not recognized*)
val parse : string -> command