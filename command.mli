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

type socket_command =
  | Host
  | Join of string

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses the terminal input into a command type. 
    Raise [Malformed] if command is not recognized. *)
val parse : string -> command

val parse_game_mode : string -> game_mode

val parse_socket : string -> socket_command