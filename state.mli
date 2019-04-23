(** 
   Representation of the state of the game. 
*)

(** The type [game_status] represents status of game. 
    It is [InProgress] if the game is still in progress; [Winner] if either (1)
    dealer is the only player that won OR (2) non-dealer player(s) won; [Draw]
    if multiple players won where one of the winners is the dealer. [Winner] and
    [Draw] commands have a string list associated which contains the names of 
    winners *)
type game_status = 
  | Winner of string list
  | InProgress
  | Draw of string list

(** The type [player_status] represents status of player. If the player's score 
    is over 21, it is [busted], if the round is still in progress it is [playing]
    , and if the player has completely their turn without busting it is [checked]*)
type player_status = 
  | Playing
  | Checked
  | Busted

(** The type [player] contains player's information and status in game *)
type player

(** The abstract type of values representing the game state. *)
type t

(** [make_player str hand] makes a new player with name [str], starting hand 
    [hand], a wallet balance of [dollars], and a bet [bet_val] *)
val make_player: string -> Game.deck -> player_status -> int -> int -> player

(** [init_state player_names] creates the initial state of the game. A new deck is
    created, two cards are handed to players in [player_names] and two cards are handed
    to the 'dealer'. The first turn goes to first player in [player_names].*)
val init_state : string list -> t

(** [get_current_player_name state] is the name of the current player in [state]*)
val get_current_player_name : t -> string

(** [get_players_list state] gets list of player*)
val get_players_list : t -> player list

(** [get_hand_of_name state] is the hand of [name] *)
val get_player_hand : t -> string -> Game.deck

(** [get_player_bet state] is the bet of [name] *)
val get_player_bet : t -> string -> int -> int

(** [get_player_wallet_by_name] is the wallet of [name] *)
val get_player_wallet : t -> string -> int

(** [get_players_of_status player_lst status] returns list of player names that 
    have player_status [status]*)
val get_players_of_status : player list -> player_status -> string list

(** [hit state] is an updated state after dealing out a card to the current player.   
    If player is still[Playing] status after new card is dealt, don't rotate turn. 
    If player is [Busted],  rotate turn to point to next player*)
val hit : t -> t 

(** [check state] is an updated state with new player status and rotated turn
    to point to next player*)
val check : t -> t

(** [bet state bet_val] is the updated state after the current player's bet has 
    been changed to [bet_val] *)
val bet : t -> int -> t

(** [check_game_status state] returns game_status according to all player's state. If at least 
    one player is [Playing], return game_status [InProgress]. If all players are [Busted], return [End].
    If there are no [Playing] player_status and at least one [Checked], return [Winner] with string
    list of player names that won.*)
val check_game_status : t -> game_status





(****************************** DISPLAY CARDS ********************************)

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


