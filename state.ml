open Game

(** The type [game_status] represents status of game*)
type game_status = 
  | Winner of string list
  | Playing
  | End (** All players busted *)

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
  wallet: int;
  bet: int;
}

(** The abstract type of values representing the game state. *)
type t = {
  players: player list;
  current_player_name: string;
  card_deck: deck;
}

(** [make_player str hand] makes a new player with name [str] and starting hand [hand] *)
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

(** [get_hand_of_current state] gets the hand of current player *)
let get_hand_of_current (state : t) : deck = 
  (get_player_by_name state (get_current_player_name state)).hand

let get_player_hand (state : t) (name : string) : deck = 
  (get_player_by_name state name).hand

let get_player_bet (state : t) (player : player) (bet : int) : int = 
  (get_player_by_name state player.name).bet

let get_current_player_wallet (state : t) : int = 
  (get_player_by_name state (get_current_player_name state)).wallet

let get_player_wallet_by_name (state : t) (player : player) : int = 
  (get_player_by_name state player.name).wallet

(** [next_player_name players current players_after_current] returns name of next player
    that is still in [Playing] status*)
let next_player_name players current players_after_current = 
  let rec next_turn lst = 
    match lst with 
    | h::t -> if h.status = Playing then h.name else next_turn t
    | [] -> next_turn players in
  next_turn players_after_current

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
            else next_player_name state.players current_player t; (* Busted -> find next player *)
          card_deck = (fst deal_to_player);
        }
      )
      else match_player t (acc@[h])
    | _ -> failwith "No such player (hit)" 
  in match_player state.players []

let next_line () = ANSITerminal.(print_string [Reset] "\n")

let print_player_wallet state name = 
  ANSITerminal.(print_string [magenta]
                  (name ^ "\'s current balance: $" ^ 
                   string_of_int (get_player_wallet_by_name state (get_player_by_name state name)) ^ ".\n"))

let show_deck (state : t) = 
  show_deck_pile state.card_deck (size state.card_deck)

let print_dealer_hand (state : t) (w : bool): unit = 
  print_player_wallet state "Dealer";
  ANSITerminal.(print_string [cyan] ("Dealer's hand:\n")); 
  if w then print_deck (get_player_hand state "Dealer") "Dealer" else
  print_deck_hide_first (get_player_hand state "Dealer") "Dealer"

let print_current_player_hand state = 
  print_player_wallet state (get_current_player_name state); 
  ANSITerminal.(print_string [cyan] (get_current_player_name state^"'s hand:\n"));
  print_deck (get_hand_of_current state) (get_current_player_name state)

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
          current_player_name = next_player_name state.players current_player t; (* find next player *)
          card_deck = state.card_deck;
        }
      )
      else match_player t (acc@[h])
    | _ -> failwith "No such player (check)" 
  in match_player state.players []

(** [get_winner players winner score] computes the names of winners of the game and returns
    Winner game_status*)
let rec get_winner players winner score = 
  match players with 
  | [] -> Winner winner
  | h::t -> let player_score = calculate_score h.hand in
    if h.status = Playing || h.status = Checked then 
      (if player_score > score then get_winner t (h.name::[]) player_score
       else if player_score = score then get_winner t (h.name::winner) score
       else get_winner t winner score)
    else get_winner t winner score

(** [check_game_status state] returns game_status according to all player's state. If at least 
    one player is [Playing], return game_status [Playing]. If all players are [Busted], return [End].
    If there are no [Playing] player_status and at least one [Checked], return [Winner] with string
    list of player names that won.*)
let check_game_status state =
  let players = state.players in
  let rec check_players players_lst all_players_busted : game_status = 
    match players_lst with
    | [] -> if all_players_busted then End else get_winner players [] 0
    | h::t when h.status=Playing -> Playing
    | h::t when h.status=Busted -> check_players t (true && all_players_busted)
    | h::t when h.status=Checked -> check_players t false
    | _ -> failwith "no match"
  in check_players players false


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
