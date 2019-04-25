(**
   Parsing of player commands.
*)

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an int phrase. *)
type command = 
  | Hit
  | Check
  | Bet of int
  | Quit

(** The type [game_mode] indicates how many players will be in game *)
type game_mode = 
  | Singleplayer
  | Multiplayer

(** The type [socket_command] indicates if you are the host or joining *)
type socket_command =
  | Host
  | Join of string

(** The type [ai_command] indicates if the AI player will join *)
type ai_command =
  | Yes
  | No

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses the terminal input into a command type. 
    Raise [Malformed] if command is not recognized. *)
val parse : string -> command

(** [parse_game_mode str] parses the terminal input into a game_mode type. 
    Raise [Malformed] if command is not recognized. *)
val parse_game_mode : string -> game_mode

(** [parse_socket str] parses the terminal input into a socket_command type. 
    Raise [Malformed] if command is not recognized. *)
val parse_socket : string -> socket_command

(** [parse_ai str] parses the terminal input into a ai_command type. 
    Raise [Malformed] if command is not recognized. *)
val parse_ai : string -> ai_command