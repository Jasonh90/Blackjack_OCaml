(** 
   Representation of the state of the game. 

   DESCRIPTION OF THE MODULE.
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

val hit : t -> t 

(* val check : t -> t *)

val print_init_hand : t -> unit