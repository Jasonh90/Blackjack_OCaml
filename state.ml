open Game

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
type player = {
  name: string;
  hand: deck;
  status: player_status;
  wallet: int;
  bet: int;
}

(** The abstract type of values representing the current game state. *)
type t = {
  players: player list;
  current_player_name: string;
  card_deck: deck;
}

(** [make_player str hand] makes a new player with name [str], starting hand 
    [hand], a wallet balance of [dollars], and a bet [bet_val] *)
let make_player str hand status dollars bet_val : player = 
  { name = str; hand = hand; status = status; wallet = dollars; bet = bet_val}

(** [init_state player_name] creates the initial state of the game. A new deck is
    created, two cards are handed to player with [player_name] and two cards are handed
    to the 'dealer'. The first turn goes to player.*)
let init_state player_name = 
  let deck = make_deck in (* initialize deck *)
  let deal_to_player = deal deck empty_deck 2 in (* new deck, player hand *)
  let new_deck = fst deal_to_player in
  let deal_to_dealer = deal new_deck empty_deck 2 in (* new deck, dealer hand *)
  (* create players *)
  let player = make_player player_name (snd deal_to_player) Playing 500 0 in
  let dealer = make_player "Dealer" (snd deal_to_dealer) Playing 5000 0 in
  (* return initialized state *)
  {
    players = [player; dealer];
    current_player_name = player.name;
    card_deck = fst deal_to_dealer;
  }

(** [get_player_by_name state] is the player whose name is [name]. *)
let get_player_by_name (state : t) (name : string) : player = 
  let rec match_player players = 
    match players with 
    | h::t -> if h.name = name then h else match_player t
    | _ -> failwith ("No such player named " ^ name)
  in match_player state.players

(** [get_current_player_name state] gets name of current player*)
let get_current_player_name (state : t) : string = 
  state.current_player_name

(** [get_players_list state] gets list of player*)
let get_players_list state = 
  state.players

let get_player_hand (state : t) (name : string) : deck = 
  (get_player_by_name state name).hand

let get_player_bet (state : t) (name : string) (bet : int) : int = 
  (get_player_by_name state name).bet

let get_player_wallet (state : t) (name : string) : int = 
  (get_player_by_name state name).wallet

(** [get_players_of_status player_lst status] returns list of player names that 
    have player_status [status]*)
let get_players_of_status player_lst status = 
  let rec match_status lst status acc = 
    match lst with 
    | [] -> acc
    | h::t -> if h.status = status then match_status t status (acc@[h.name]) 
      else match_status t status acc in
  match_status player_lst status []

(** [next_player_name players current players_after_current] returns name of next player
    that is still in [Playing] status*)
let next_player_name current players_after_current = 
  let next_players = get_players_of_status players_after_current Playing in
  match next_players with 
  | h::t -> h
  | [] -> ""

(** [hit state] returns an updated state after dealing out a card to the player. If player is still
    [Playing] status after new card is dealt, don't rotate turn. If player is [Busted],  rotate turn
    to point to next player*)
let hit state = 
  let current_player = state.current_player_name in
  let rec match_player players acc = (* find current player and deal out a new card *)
    match players with 
    | h::t -> if h.name = current_player then (
        let deal_to_player = deal state.card_deck h.hand 1 in (* new deck, new player hand *)
        let new_status = if calculate_score (snd deal_to_player) > 21 then Busted else Playing in (* determine new status *)
        let players = acc@[make_player (h.name) (snd deal_to_player) new_status h.wallet h.bet]@t in (* update current player's hand and status *)
        (** updated state *)
        {
          players = players;
          current_player_name = if new_status = Playing then current_player (* Playing -> don't change turns *)
            else next_player_name current_player t; (* Busted -> find next player *)
          card_deck = (fst deal_to_player);
        }
      )
      else match_player t (acc@[h])
    | _ -> failwith "No such player (hit)" 
  in match_player state.players []

(** [check state] returns an updates state with new player status. Also rotates turn
    to point to next player*)
let check state =
  let current_player = state.current_player_name in
  let rec match_player players acc = (* find current player and deal out a new card *)
    match players with 
    | h::t -> if h.name = current_player then (
        let players = acc@[make_player (h.name) h.hand Checked h.wallet h.bet]@t in (* update current player's status *)
        (** updated state *)
        {
          players = players;
          current_player_name = next_player_name current_player t; (* find next player *)
          card_deck = state.card_deck;
        }
      )
      else match_player t (acc@[h])
    | _ -> failwith "No such player (check)" 
  in match_player state.players []

(** [bet state] is an updated state with the current players bet updated to be bet_val*)
let bet (state : t) (bet_val : int) : t = 
  let current_player = get_current_player_name state in
  let rec match_player players acc = (* find current player and deal out a new card *)
    match players with 
    | h::t -> if h.name = current_player then (
        let player_dollars = h.wallet - bet_val in (* new deck, new player hand *) 
        (* update current player's hand and status *)
        let players = acc@[make_player h.name h.hand h.status player_dollars bet_val]@t in
        (** updated state *)
        {
          players = players;
          current_player_name = current_player;
          card_deck = state.card_deck;
        }
      )
      else match_player t (acc@[h])
    | _ -> failwith "No such player: Can't bet without a player" 
  in match_player state.players []

(** [get_winner players winner score] computes the names of winners of the game and returns
    [Winner] or [Draw] game status*)
let rec get_winner players winner score blackjack: game_status = 
  match players with 
  | [] ->  
    if List.exists (fun name -> name="Dealer") winner && List.length winner <> 1
    then 
      (* all players that drawed with dealer *)
      Draw (List.filter (fun name -> name<>"Dealer") winner)
    else Winner winner
  | h::t -> if h.status = Busted then get_winner t winner score blackjack (* player busted *)
    else ((* player not busted *)
      if blackjack then (* Blackjack already exists *)
        if has_blackjack h.hand then get_winner t (h.name::winner) score blackjack (* player has blackjack *)
        else get_winner t winner score blackjack
      else (* no blackjack yet *)
        let player_score = calculate_score h.hand in
        if has_blackjack h.hand then get_winner t (h.name::[]) player_score true (* player has blackjack *)
        else
          (if player_score > score then get_winner t (h.name::[]) player_score blackjack
           else if player_score = score then get_winner t (h.name::winner) score blackjack
           else get_winner t winner score blackjack)
    )

(** [get_state_of_dealer state] gets the status of dealer. *)
let get_state_of_dealer state = 
  let rec match_dealer players = (* find current player and return player's hand *)
    match players with 
    | h::t -> if h.name = "Dealer" then h.status else match_dealer t
    | _ -> failwith "No such player (state of dealer)" 
  in match_dealer state.players

(** [check_game_status state] returns game_status according to all player's state. If at least 
    one player is [Playing], return game_status [InProgress]. Else, if dealer is [Busted] return [Winner]
    with all players that are not busted. If dealer is not [Busted], return [Winner] if dealer lost or 
    dealer is the only winner; return [Draw] if there are players that drawed with dealer *)
let check_game_status state =
  let players = state.players in
  let rec check_players players_lst : game_status = 
    match players_lst with
    | [] -> (
        match (get_state_of_dealer state) with
        | Checked -> get_winner players [] 0 false
        | Busted -> Winner (get_players_of_status players Checked) (* Dealer Busted - All players who didn't bust yet wins *)
        | _ -> failwith ""
      )
    | h::t when h.status=Playing -> InProgress
    | h::t when h.status=Busted -> check_players t
    | h::t when h.status=Checked -> check_players t
    | _ -> failwith "no match"
  in check_players players





(****************************** DISPLAY CARDS ********************************)

let next_line () = ANSITerminal.(print_string [Reset] "\n")

let print_player_wallet state name = 
  ANSITerminal.(print_string [magenta]
                  (name ^ "\'s current balance: $" ^ 
                   string_of_int (get_player_wallet state name) ^ ".\n"))

let show_deck (state : t) = 
  show_deck_pile state.card_deck (size state.card_deck)

let print_dealer_hand (state : t) (w : bool): unit = 
  print_player_wallet state "Dealer";
  ANSITerminal.(print_string [cyan] ("Dealer's hand:\n")); 
  if w then print_deck (get_player_hand state "Dealer") "Dealer" else
    print_deck_hide_first (get_player_hand state "Dealer") "Dealer"

let print_current_player_hand state = 
  let current = state.current_player_name in
  print_player_wallet state (current); 
  ANSITerminal.(print_string [cyan] (current^"'s hand:\n"));
  print_deck (get_player_hand state current) (current)

let print_player_hand state player = 
  print_player_wallet state player.name; 
  ANSITerminal.(print_string [cyan] (player.name^"'s hand:\n"));
  print_deck player.hand player.name

let print_dealer_hidden (state : t) : unit = 
  print_dealer_hand state false;
  show_deck state;
  print_current_player_hand state

let print_players_cept_dealer state = function
  | h::t when h.name <> "Dealer" -> print_player_hand state h;
  | _ -> ()

let print_winner (state : t) = 
  print_dealer_hand state true;
  show_deck state;
  print_players_cept_dealer state state.players
(* let rec print = function
   | h::t -> print_deck (get_player_hand state h.name) (h.name); print t
   | [] -> ()
   in print (List.rev state.players) *)



