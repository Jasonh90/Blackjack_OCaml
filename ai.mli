open Command
open Game
open State

(** 
   Contains functions that contribute to configuring the algorithms for
   dealer and AI player.
*)

(** [dealer player_lst hand] determines the moves for dealer. If all players are [Busted] 
    dealer calls check. Otherwise, when dealer has cards otaling to 16 or less, 
    dealer calls hit. If not, dealer calls check. *)
val dealer : player list -> deck -> command