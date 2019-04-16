(** 
   Representation of the state of the game. 
*)

(** The abstract type of values representing the game state. *)
type t

(** The type [game_status] represents status of game*)
type game_status = 
  | Winner of string list
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

(** [get_hand_of_current state] gets the hand of current player *)
val get_hand_of_current : t -> Game.deck

(** [get_hand_of_name state] gets the hand of [name] *)
val get_hand_of_name : t -> string -> Game.deck

(** [get_current_player_name state] gets name of current player*)
val get_current_player_name : t -> string

(** [hit state] returns an updated state after dealing out a card to the player. If player is still
    [Playing] status after new card is dealt, don't rotate turn. If player is [Busted],  rotate turn
    to point to next player*)
val hit : t -> t 

val print_hands : t -> unit

val print_winner : t -> unit

(** [check state] returns an updates state with new player status. Also rotates turn
    to point to next player*)
val check : t -> t

(** [check_game_status state] returns game_status according to all player's state. If at least 
    one player is [Playing], return game_status [Playing]. If all players are [Busted], return [End].
    If there are no [Playing] player_status and at least one [Checked], return [Winner] with string
    list of player names that won.*)
val check_game_status : t -> game_status


val show_deck : t -> unit