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