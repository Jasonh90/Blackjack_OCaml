(** 
   Representation of the state of the game. 
*)

(** The abstract type of values representing the game state. *)
type t

type player

(** [init_state player_name] creates the initial state of the game. A new deck is
    created, two cards are handed to player with [player_name] and two cards are handed
    to the 'dealer'. The first turn goes to player.*)
val init_state : string -> t

(** [get_hand state] gets the hand of current player *)
val get_hand : t -> Adventure.deck

(** [hit state] returns a new state after dealing out a card to the player *)
val hit : t -> t 

(* val check : t -> t *)

val print_init_hand : t -> unit
(** [check state] returns a new state with no change in player's hand*)
val check : t -> t
