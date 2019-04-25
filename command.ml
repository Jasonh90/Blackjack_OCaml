(** The type [command] represents a player command that is decomposed
    into a verb and possibly an int phrase. *)
type command = 
  | Hit
  | Check
  | Bet of int
  | Quit

(** The type [game_mode] is Singleplayer if the user wants to play alone and 
    Multiplayer if they would like to Join or Host*)
type game_mode = 
  | Singleplayer
  | Multiplayer

(** The type [socket_command] indicates if you are the host or joining *)
type socket_command =
  | Host
  | Join of string

(** The type [ai_command] indicates is Yes if the player wants the AI player to 
    join the game and No otherwise*)
type ai_command =
  | Yes
  | No

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses the terminal input into a [command] type. 
    Raise [Malformed] if command is not recognized. *)
let parse str = 
  match Str.split (Str.regexp " ") (String.lowercase_ascii str) with 
  | ["hit"] -> Hit
  | ["check"] -> Check
  | ["quit"] -> Quit
  | ["bet";n] -> (try Bet (int_of_string n) with Failure _ -> raise Malformed)
  | _ -> raise Malformed

(** [parse str] parses the terminal input into a [game_mode] type. 
    Raise [Malformed] if command is not recognized. *)
let parse_game_mode str = 
  match Str.split (Str.regexp " ") (String.lowercase_ascii str) with 
  | ["single"] -> Singleplayer
  | ["multi"] -> Multiplayer
  | _ -> raise Malformed

(** [parse str] parses the terminal input into a [socket_command] type. 
    Raise [Malformed] if command is not recognized. *)
let parse_socket str = 
  match Str.split (Str.regexp " ") (String.lowercase_ascii str) with 
  | ["host"] -> Host
  | ["join"; n] -> Join n
  | _ -> raise Malformed

(** [parse str] parses the terminal input into a [ai_command] type. 
    Raise [Malformed] if command is not recognized. *)
let parse_ai str = 
  match Str.split (Str.regexp " ") (String.lowercase_ascii str) with 
  | ["yes"] -> Yes
  | ["no"] -> No
  | _ -> raise Malformed