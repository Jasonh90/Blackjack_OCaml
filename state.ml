open Game

(** The type [game_status] represents status of game*)
type game_status = 
  | Winner of string list
  | Playing
  | Draw of string list

(** The type [player_status] represents status of player*)
type player_status = 
  | Playing
  | Checked
  | Busted

(** The type [player] contains player's information and status in game *)
type player = {
  name: string;
  hand: deck;
  status: player_status;
}

(** The abstract type of values representing the game state. *)
type t = {
  players: player list;
  current_player_name: string;
  card_deck: deck;
}

(** [make_player str hand] makes a new player with name [str] and starting hand [hand] *)
let make_player str hand status  : player = 
  { name = str; hand = hand; status = status}

(** [init_state player_name] creates the initial state of the game. A new deck is
    created, two cards are handed to player with [player_name] and two cards are handed
    to the 'dealer'. The first turn goes to player.*)
let init_state player_name = 
  let deck = make_deck in (* initialize deck *)
  let deal_to_player = deal deck empty_deck 2 in (* new deck, player hand *)
  let new_deck = fst deal_to_player in
  let deal_to_dealer = deal new_deck empty_deck 2 in (* new deck, dealer hand *)
  (* create players *)
  let player = make_player player_name (snd deal_to_player) Playing in
  let dealer = make_player "Dealer" (snd deal_to_dealer) Playing in
  (* return initialized state *)
  {
    players = [player; dealer];
    current_player_name = player.name;
    card_deck = fst deal_to_dealer;
  }

(** [get_hand_of_current state] gets the hand of current player *)
let get_hand_of_current state = 
  let current_player = state.current_player_name in
  let rec match_player players = (* find current player and return player's hand *)
    match players with 
    | h::t -> if h.name = current_player then h.hand else match_player t
    | _ -> failwith "No such player (hand of current)" 
  in match_player state.players

(** [get_hand_of_name state] gets the hand of [name]. *)
let get_hand_of_name state name = 
  let rec match_player players = 
    match players with 
    | h::t -> if h.name = name then h.hand else match_player t
    | _ -> failwith "No such player (hand of name)" 
  in match_player state.players

(** [get_current_player_name state] gets name of current player*)
let get_current_player_name state = 
  state.current_player_name

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
        let players = acc@[make_player (h.name) (snd deal_to_player) new_status]@t in (* update current player's hand and status *)
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

let show_deck (state : t) = show_deck_pile state.card_deck (size state.card_deck)

let print_hands (state : t) : unit = 
  print_deck_hide_first (get_hand_of_name state "Dealer") "Dealer";
  show_deck state;
  print_deck (get_hand_of_current state) (get_current_player_name state)

let print_winner state = 
  let rec print = function
    | h::t -> print_deck (get_hand_of_name state h.name) (h.name); print t
    | [] -> ()
  in print (List.rev state.players)

(** [check state] returns an updates state with new player status. Also rotates turn
    to point to next player*)
let check state =
  let current_player = state.current_player_name in
  let rec match_player players acc = (* find current player and deal out a new card *)
    match players with 
    | h::t -> if h.name = current_player then (
        let players = acc@[make_player (h.name) h.hand Checked]@t in (* update current player's status *)
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
    one player is [Playing], return game_status [Playing]. Else, if dealer is [Busted] return [Winner]
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
    | h::t when h.status=Playing -> Playing
    | h::t when h.status=Busted -> check_players t
    | h::t when h.status=Checked -> check_players t
    | _ -> failwith "no match"
  in check_players players

