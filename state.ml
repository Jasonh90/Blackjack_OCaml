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

(** this type contains information about cards that have been played already*)
type used_deck = {
  used_cards: (int * int) list;
  total_left: int;
}

(** The abstract type of values representing the current game state. *)
type t = {
  players: player list;
  current_player_name: string;
  card_deck: deck;
  used: used_deck;
}

(** [restart] is a new used_deck with no bindings to be used 
    when the deck is reshuffled*)
let restart () : used_deck = {used_cards = []; total_left = 52;}

(** [add_deck lst hand] is [lst] with the (num,1) appended if num was not already 
    in [lst] and (num,x+1) appended to replace the pair (num, x) if num was already 
    a member of [lst] where num is the number of each card in [hand]*)
let rec add_deck lst hand = 
  match hand with 
  |[] -> lst
  | h :: t -> if not (List.mem_assoc (get_number h) lst) then add_deck (((get_number h),1) :: lst) t else
      let current = List.assoc (get_number h) lst in 
      let new_lst = List.remove_assoc (get_number h) lst in 
      add_deck (((get_number h), current + 1) :: new_lst) t

(** [add_used_cards lst players] is [lst] with the numbers from the hand of each
    player in [players] appended*)
let rec add_used_cards used (players : player list) = 
  match players with
  |[] -> used
  |h :: t -> let old_used_cards = used.used_cards in 
    add_used_cards {used with used_cards= (add_deck old_used_cards (Game.deck_to_list (h.hand)))} t

let get_used_cards used = used.used_cards

let get_total_left used = used.total_left




(** [cards_in_play s] is the current cards in play. *)
let rec cards_in_play (state : t) : deck  = 
  let rec add_cards acc = function
    | [] -> acc
    | h::t -> add_cards (combine_cards h.hand acc) t
  in add_cards empty_deck state.players

(** [make_player str hand] makes a new player with name [str], starting hand 
    [hand], a wallet balance of [dollars], and a bet [bet_val] *)
let make_player str hand status dollars bet_val : player = 
  { name = str; hand = hand; status = status; wallet = dollars; bet = bet_val}

(** [init_state player_name] creates the initial state of the first game. A new deck is
    created, two cards are handed to player with [player_name] and two cards are handed
    to the 'dealer'. The first turn goes to player.*)
let init_state player_name = 
  let deck = make_deck in (* initialize deck *)
  let deal_to_player = deal deck empty_deck empty_deck 2 in (* new deck, player hand *)
  let new_deck = fst deal_to_player in
  let deal_to_dealer = deal new_deck empty_deck empty_deck 2 in (* new deck, dealer hand *)
  (* create players *)
  let player = make_player player_name (snd deal_to_player) Playing 500 0 in
  let dealer = make_player "Dealer" (snd deal_to_dealer) Playing 5000 0 in
  (* return initialized state *)
  {
    players = [player; dealer];
    current_player_name = player.name;
    card_deck = fst deal_to_dealer;
    used = restart ();
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

let get_player_bet (state : t) (name : string) : int = 
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
        let deal_to_player = deal state.card_deck h.hand (cards_in_play state) 1 in (* new deck, new player hand *)
        let new_status = if calculate_score (snd deal_to_player) > 21 then Busted else Playing in (* determine new status *)
        let players = acc@[make_player (h.name) (snd deal_to_player) new_status h.wallet h.bet]@t in (* update current player's hand and status *)
        (** updated state *)
        {state with
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
        { state with
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
        {state with
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

(** [change_players_to_playing status state] is the [state] with all the players' 
    statuses changed to [status]. *)
let change_players_statuses_to (status : player_status) (state : t) : t = 
  let rec change acc = function
    | [] -> acc
    | h::t -> change ({h with status = status}::acc) t
  in {state with players = (change [] state.players)}

(** [pay_up state winners] is the updated state after a round. Each winner earns 
    their bet value while the dealer earns each bet that each players loses. The 
    order of the players' list is not maintained. This only pays the respective 
    players their portion of the money. None of the other parts of the [state]
    is altered. 
    Example: "Jason" bets 55 & gets blackjack, "Dealer" busts -> "Jason" earns 55
    "Jason" bets 13 and busts -> "Dealer" earns 13 *)
let rec pay_up (state : t) (winners : string list) : t = 
  (* Update all players but the dealer, get the dealer's earnings/losings *)
  let rec update_players (acc : player list) (dealers_wallet : int) = function
    (* When this happens, acc should have all the players. *)
    | [] when acc == [] -> failwith "acc is empty"
    | [] -> acc, dealers_wallet
    (* When a player won -> they earn 2x their bet, dealer loses 1x bet *)
    | h::t when List.mem h.name winners && h.name <> "Dealer" -> 
      (* If dealer is in the winners list, then the player [h] receives their original
          bet, but if dealer is not in the list, then [h] gets twice their bet. (Their 
          wallet is already subtracted of their bet to begin with.) *)
      let updated_wallet = h.wallet + h.bet * (if List.mem "Dealer" winners then 1 else 2) in
      update_players ({h with wallet = updated_wallet; bet = 0}::acc) 
        (dealers_wallet - h.bet) t
    (* When a player lost -> they lose their bet, dealer earns *)
    | h::t when h.name <> "Dealer" -> update_players ({h with bet = 0}::acc) (dealers_wallet + h.bet) t
    (* Skip Dealer, add onto acc *)
    | h::t -> update_players (h::acc) dealers_wallet t
  in 
  let players, d_wallet = update_players [] (get_player_wallet state "Dealer") (List.rev state.players) in 
  (* Pay the dealer with the updated dealer wallet from the previous function. *)
  let rec pay_dealer (state : t) acc = function
    (* There shouldn't be a case when state.players is [] *)
    | [] -> failwith "No players."
    (* Return the new state with dealer's wallet updated *)
    | h::t when h.name = "Dealer" -> {state with players = (acc@{h with bet = 0; wallet = d_wallet}::t)}
    (* Not dealer yet, keep looping... *)
    | h::t -> pay_dealer state (acc@[h]) t 
  in pay_dealer state [] players

(** [update_state s] is the updated state [s] where each player's hands are 
    redealt. If there's not enough cards in the deck, the deck will be re-shuffled
    without the current cards in play. The order of the players are not maintained. 
    This sets up the game for the next round. Each player's status is now Playing. *)
let update_state (state : t) : t = 
  (* [update_players acc deck] is the updated player list. *)
  let rec update_players acc deck = function
    | player::players -> 
      (* deal takes care of not enough cards to give out. *)
      let deal_to_player = deal deck empty_deck (cards_in_play state) 2 in 
      let new_deck = fst deal_to_player in
      (* let deal_to_dealer = deal new_deck empty_deck (cards_in_play state) 2 in  *)
      let updated_player = {player with hand = (snd deal_to_player); status = Playing} in
      (* let dealer = make_player "Dealer" (snd deal_to_dealer) Playing player.wallet 0 in *)
      update_players (acc@[updated_player]) new_deck players
    | [] -> acc,deck
  in let updated_players, updated_deck = update_players [] state.card_deck state.players in
  {state with
   players = updated_players;
   current_player_name = (List.hd updated_players).name; 
   card_deck = updated_deck
  }






(** [get_hand player] is the hand of [player]*)
let get_hand player =
  player.hand 

(* WHEN RESHUFFLING, TELL AI SOMEHOW THAT THE GAME RESHUFFLED THE DECK. *)

(****************************** DISPLAY CARDS ********************************)

let get_current_bet state = 
  let rec current_bet acc = function 
    | [] -> acc 
    | h::t -> current_bet (acc + h.bet) t 
  in current_bet 0 state.players

let print_curr_bet v = 
  ANSITerminal.(print_string [cyan;Bold] ("   Current bet: $" ^ string_of_int v))

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
  print_curr_bet (get_current_bet state);
  show_deck state;
  print_current_player_hand state

let rec print_players_cept_dealer state = function
  | h::t when h.name <> "Dealer" -> 
    print_player_hand state h; print_players_cept_dealer state t
  | h::t -> print_players_cept_dealer state t
  | _ -> ()

let print_winner (state : t) = 
  print_dealer_hand state true;
  show_deck state;
  print_players_cept_dealer state state.players
(* let rec print = function
   | h::t -> print_deck (get_player_hand state h.name) (h.name); print t
   | [] -> ()
   in print (List.rev state.players) *)



