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

(** this type gives information about the cards played already*)
type used_deck

(** The abstract type of values representing the game state. *)
type t

(** [restart] creates a new empty type used_deck*)
val restart : unit -> used_deck

(** [add_used_cards] adds the cards from each players hand into the association
    list recording played cards *)
val add_used_cards : used_deck -> player list -> used_deck

(** [get_used_cards] is the used_cards of [used] *)
val get_used_cards : used_deck -> (int * int) list

(** [get_total_left] is the total_left of [used]*)
val get_total_left : used_deck -> int

(** [get_used state] is the used_deck used of state*)
val get_used : t -> used_deck

(** [make_player str hand] makes a new player with name [str], starting hand 
    [hand], a wallet balance of [dollars], and a bet [bet_val] *)
val make_player: string -> Game.deck -> player_status -> int -> int -> player

(** [init_state player_names has_ai] creates the initial state of the game. A new deck is
    created, two cards are handed to players in [player_names] and two cards are handed
    to the 'dealer'. The first turn goes to first player in [player_names].
    If [has_ai] is true, add AI player to list of players. *)
val init_state : string list -> bool -> t

(** [get_current_player_name state] is the name of the current player in [state]*)
val get_current_player_name : t -> string

(** [get_players_list state] gets list of player*)
val get_players_list : t -> player list

(** [get_hand_of_name state] is the hand of [name] *)
val get_player_hand : t -> string -> Game.deck

(** [get_player_bet state name] is the bet of [name] *)
val get_player_bet : t -> string -> int

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

(** [bet state bet_val name] is an updated state with the [name] player's bet 
    updated to be [bet_val]*)
val bet : t -> int -> string -> t

(** [check_game_status state] returns game_status according to all player's state. 
    If at least one player is [Playing], return game_status [InProgress]. 
    Else, if dealer is [Busted] return [Winner] with all players that are 
    not busted. If dealer is not [Busted], return [Winner] if dealer lost or dealer 
    is the only winner; return [Draw] if there are players that drawed with dealer *)
val check_game_status : t -> game_status

(** [pay_up state winners] is the updated state after a round. Each winner earns 
    their bet value while the dealer earns each bet that each players loses. The 
    order of the players' list is not maintained. This only pays the respective 
    players their portion of the money. None of the other parts of the [state]
    is altered. 
    Example: "Jason" bets 55 & gets blackjack, "Dealer" busts -> "Jason" earns 55
    "Jason" bets 13 and busts -> "Dealer" earns 13 *)
val pay_up : t -> string list -> t

(** [update_state s] is the updated state [s] where each player's hands are 
    redealt. If there's not enough cards in the deck, the deck will be re-shuffled
    without the current cards in play. The order of the players are not maintained. 
    This sets up the game for the next round.
    Requires: [s] has its players updated with the correct money values. *)
val update_state : t -> t

(****************************** SOCKET USAGE *********************************)

(** [state_of_string str] parses a string and makes it into a state type t *)
val state_of_string : string -> t

(** [string_of_state s] is the string representation of the state [s]. Each item 
    of state is delimited with "/" while each list such as player list has 
    delimiters of "&". Each nested item is delimited with ";". *)
val string_of_state : t -> string

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
