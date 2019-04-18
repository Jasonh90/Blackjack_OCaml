(** 
   Representation of game functions.

   DESCRIPTION OF THIS MODULE.
*)

(** The abstract type of values representing card suits. *)
type suit

(** The abstract type of values representing a single card. *)
type card

(** The abstract type of values representing a deck of cards. *)
type deck

exception EmptyDeck

(** [make_deck] is a full shuffled deck of cards *)
val make_deck : deck

(** [empty_deck] is a deck containing 0 cards *)
val empty_deck : deck

(** [shuffle lst] is a random permutation of the deck [lst]. *)
val shuffle : deck -> deck

(** [deak deck hand num] is a tuple containing the remaining deck and the 
    resulting hand from dealing [num] cards into [hand]*)
val deal : deck -> deck -> int -> deck*deck

(** [calculate_score hand] is the total of the cards in [hand]*)
val calculate_score : deck -> int

(** [has_blackjack hand] returns boolean to indicate that the hand is blackjack *)
val has_blackjack : deck -> bool

(** [size deck] is the number of cards in [deck]. *)
val size : deck -> int

(** [print_deck deck] is the [deck] shown side by side on screen. *)
val print_deck : deck -> string -> unit

(** [print_deck_col deck] is the [deck] shown in a column on screen. *)
val print_deck_col : deck -> unit

(** [print_last_card deck] is the last card in the [deck] on screen. *)
val print_last_card : deck -> unit

(** [print_deck_hide_first deck name] is the [deck] with the first card 
    hidden on screen with the corresponding [name]. *)
val print_deck_hide_first : deck -> string -> unit

(** [show_deck_pile deck num] prints on the screen the [deck] facing down with the 
    total number of cards [num] showing on top. *)
val show_deck_pile : deck -> int -> unit