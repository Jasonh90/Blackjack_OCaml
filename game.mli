(** 
   Collection of game functions.
*)

(** The abstract type of values representing card suits. *)
type suit = Clubs | Diamonds | Hearts | Spades

(** The abstract type of values representing a single card. *)
type card

(** The abstract type of values representing a deck of cards. *)
type deck

(** Raised when function is trying to perform action on an empty deck. *)
exception EmptyDeck

(** [deck_test num suit deck] add a card with number [num] and suit [suit] 
     to [deck]. This function is solely for testing. *)
val deck_test : int -> suit -> deck -> deck

(** [shuffle lst] is a random permutation of the deck [lst]. *)
val shuffle : deck -> deck

(** [make_deck] is a full shuffled deck of cards *)
val make_deck : deck

(** [empty_deck] is a deck containing 0 cards *)
val empty_deck : deck

(** [size deck] is the number of cards in [deck]. *)
val size : deck -> int

(** [deal deck hand num] is a tuple containing the remaining deck and the 
    resulting hand from dealing [num] cards into [hand]*)
val deal : deck -> deck -> int -> deck*deck

(** [calculate_score hand] is the total value of the cards in [hand] counting cards 
    10-13 to be worth 10 points and aces to be worth either 1 or 11 points. *)
val calculate_score : deck -> int

(** [has_blackjack hand] is true if [hand] has a blackjack (face card & ace) *)
val has_blackjack : deck -> bool

(** [get_number card] is the number of [card] *)
val get_number : card -> int






(****************************** DISPLAY CARDS ********************************)

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