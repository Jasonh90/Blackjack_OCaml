(** 
   Representation of static adventure data.

   DESCRIPTION OF THIS MODULE.
*)

(** The abstract type of values representing adventures. *)
type suit

type card

type deck

type player

val make_deck : deck

val shuffle : deck -> deck

val deal : deck -> deck

val hit : deck -> deck -> deck

val total: int -> deck -> int
