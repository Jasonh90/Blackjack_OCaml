(** 
   Representation of the state of the game. 
*)

(** The abstract type of values representing the game state. *)
type t

(** The type [game_status] represents status of game*)
type game_status = 
  | Winner of string
  | Playing
  | End (** All players busted *)

(** The type [status] represents status of player*)
type player_status = 
  | Playing
  | Checked
  | Busted

(** The type [player] contains player's information and status in game *)
type player

(** [init_state player_name] creates the initial state of the game. A new deck is
    created, two cards are handed to player with [player_name] and two cards are handed
    to the 'dealer'. The first turn goes to player.*)
val init_state : string -> t

(** [get_hand state] gets the hand of current player *)
val get_hand : t -> Adventure.deck

(** [hit state] returns a new state after dealing out a card to the player *)
val hit : t -> t 

val print_init_hand : t -> unit

(** [check state] returns a new state with no change in player's hand*)
val check : t -> t

(** [next_turn state] returns new state with updated [current_player_name] field to point to next player*)
val next_turn : t -> t
