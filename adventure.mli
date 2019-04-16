(** 
   Representation of static adventure data.

   DESCRIPTION OF THIS MODULE.
*)

(** The abstract type of values representing adventures. *)
type suit

type card

type deck

exception EmptyDeck

val make_deck : deck

val empty_deck : deck

val shuffle : deck -> deck

val deal : deck -> deck -> int -> deck*deck

val calculate_score : deck -> int

val shuffle : deck -> deck

(** [print_deck deck] is the [deck] shown side by side on screen. *)
val print_deck : deck -> unit

(** [print_deck_col deck] is the [deck] shown in a column on screen. *)
val print_deck_col : deck -> unit

(** [print_last_card deck] is the last card in the [deck] on screen. *)
val print_last_card : deck -> unit

(** [print_deck_hide_first deck] is the [deck] with the first card hidden on screen . *)
val print_deck_hide_first : deck -> unit