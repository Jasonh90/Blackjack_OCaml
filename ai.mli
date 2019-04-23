open Command
open Game
open State

(** 
   Contains functions that contribute to configuring the algorithms for
   dealer and AI player.
*)

(** [dealer player_lst hand] determines the moves for dealer. If all players are [Busted] 
    dealer calls check. Otherwise, when dealer has cards totaling to 16 or less, 
    dealer calls hit. If not, dealer calls check. *)
val dealer : player list -> deck -> command

type used_deck

val add_used_cards : used_deck -> State.player list -> used_deck

val valid_cards : Game.deck -> int list

val calc_prob : int -> used_deck -> float

val calc_total_prob : int list -> used_deck -> float -> float

val ai_turn : used_deck -> Game.deck -> float -> command

val restart : unit -> used_deck