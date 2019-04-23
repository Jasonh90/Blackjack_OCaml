(** The type [command] represents a player command that is decomposed
    into a verb and possibly an int phrase. *)
type command = 
  | Hit
  | Check
  | Bet of int
  | Quit

type game_mode = 
  | Singleplayer
  | Multiplayer

type socket_command =
  | Host
  | Join of string

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses the terminal input into a command type. 
    Raise [Malformed] if command is not recognized. *)
let parse str = 
  match Str.split (Str.regexp " ") (String.lowercase_ascii str) with 
  | ["hit"] -> Hit
  | ["check"] -> Check
  | ["quit"] -> Quit
  | ["bet";n] -> (try Bet (int_of_string n) with Failure _ -> raise Malformed)
  | _ -> raise Malformed

let parse_game_mode str = 
  match Str.split (Str.regexp " ") (String.lowercase_ascii str) with 
  | ["single"] -> Singleplayer
  | ["multi"] -> Multiplayer
  | _ -> raise Malformed

let parse_socket str = 
  match Str.split (Str.regexp " ") (String.lowercase_ascii str) with 
  | ["host"] -> Host
  | ["join"; n] -> Join n
  | _ -> raise Malformed