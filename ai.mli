open Command
open Game
open State

(** 
   Contains functions that contribute to configuring the algorithms for
   dealer and AI player.
*)

(** [dealer player_lst hand] determines the moves for dealer. If all players 
    are [Busted] dealer calls check. Otherwise, when dealer has cards 
    totaling to 16 or less, dealer calls hit. If not, dealer calls check. *)
val dealer : player list -> deck -> State.t -> State.t

(** [valid_cards hand] is a list containing all the numbers that a player could 
    receive on a hit without busting *)
val valid_cards : deck -> int list

(** [calc_prob x lst len] is a float containing the probability of the next 
    card having the number [x] where [len] is the number of cards left in the
    deck and [lst] gives information on the cards used already *)
val calc_prob : int -> used_deck -> float

(** [calc_total_prob valid lst len sum] is a float containing the probability 
    of the next card having any number in the int lst [valid] where [len] is 
    the number of cards left in the deck and [lst] gives information on the
    cards used already *)
val calc_total_prob : int list -> used_deck -> float -> float

(** [ai_turn used hand accuracy] calculates the probability that the next card 
    pulled will be a valid card and hits if this probability is greater than 
    [accuracy] otherwise the ai player checks *)
val ai_turn : used_deck -> deck -> float -> State.t -> State.t
