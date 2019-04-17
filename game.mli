(** 
   Representation of game functions.

   DESCRIPTION OF THIS MODULE.
*)

(** The abstract type of values representing card suits. *)
type suit

type card

type deck

exception EmptyDeck

val make_deck : deck

val empty_deck : deck

val shuffle : deck -> deck

val deal : deck -> deck -> int -> deck*deck

val calculate_score : deck -> int

(** [has_blackjack hand] returns boolean to indicate that the hand is blackjack *)
val has_blackjack : deck -> bool

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