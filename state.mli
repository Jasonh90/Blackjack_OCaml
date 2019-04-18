(** 
   Representation of the state of the game. 
*)

(** The abstract type of values representing the game state. *)
type t

(** The type [game_status] represents status of current round*)
type game_status = 
  | Winner of string list
  | Playing
  | Draw of string list

(** The type [status] represents status of player's hand*)
type player_status = 
  | Playing
  | Checked
  | Busted

(** The type [player] contains player's information and status in game *)
type player

(** [init_state player_name] creates the initial state of the game. A new deck is
    created, two cards are handed to player with [player_name] and two cards are handed
    to the 'dealer'. The first turn goes to player.*)
val init_state : string -> t

(** [get_hand_of_current state] is the hand of current player in [state] *)
val get_hand_of_current : t -> Game.deck

(** [get_player_by_name state] is the player whose name is [name]. *)
val get_player_by_name : t -> string -> player

(** [get_current_player_name state] is the name of the current player in [state]*)
val get_current_player_name : t -> string

(** [get_hand_of_current state] is the hand of current player in [state] *)
val get_hand_of_current : t -> Game.deck

(** [get_hand_of_name state] is the hand of [name] *)
val get_player_hand : t -> string -> Game.deck

(** [get_player_bet state] is the bet of [name] *)
val get_player_bet : t -> player -> int -> int

(** [get_current_player_wallet state] is the wallet of the current player in [state] *)
val get_current_player_wallet : t -> int

(** [get_player_wallet_by_name] is the wallet of [name] *)
val get_player_wallet_by_name : t -> player -> int

(** [hit state] is an updated state after dealing out a card to the current player.   
    If player is still[Playing] status after new card is dealt, don't rotate turn. 
    If player is [Busted],  rotate turn to point to next player*)
val hit : t -> t 

(** [bet state bet_val] is the updated state after the current player's bet has 
    been changed to [bet_val] *)
val bet : t -> int -> t

(** [print_winner state] prints the name(s) of the winners *)
val print_winner : t -> unit

(** [print_current_player_hand state] is the hand of the current player in [state] *)
val print_current_player_hand : t -> unit

(** [show_deck state] reveals the game deck *)
val show_deck : t -> unit

(** [print_dealer_hand state] prints the dealers hand *)
val print_dealer_hand : t -> bool -> unit

(** [get_hand_of_name state] prints the one hidden card in the dealers hand *)
val print_dealer_hidden : t -> unit

(** [check state] is an updated state with new player status and rotated turn
    to point to next player*)
val check : t -> t

(** [check_game_status state] returns game_status according to all player's state. If at least 
    one player is [Playing], return game_status [Playing]. If all players are [Busted], return [End].
    If there are no [Playing] player_status and at least one [Checked], return [Winner] with string
    list of player names that won.*)
val check_game_status : t -> game_status
