open Adventure

(** The type [game_status] represents status of game*)
type game_status = 
  | Winner of string
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
}

(** The abstract type of values representing the game state. *)
type t = {
  players: player list;
  current_player_name: string;
  card_deck: deck;
}

(** [make_player str hand] makes a new player with name [str] and starting hand [hand] *)
let make_player str hand status: player = 
  { name = str; hand = hand ; status = status}


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
  let dealer = make_player "dealer" (snd deal_to_dealer) Playing in
  (* return initialized state *)
  {
    players = [player; dealer];
    current_player_name = player.name;
    card_deck = fst deal_to_dealer;
  }


(** [get_hand state] gets the hand of current player *)
let get_hand state = 
  let current_player = state.current_player_name in
  let rec match_player players = (* find current player and return player's hand *)
    match players with 
    | h::t -> if h.name = current_player then h.hand else match_player t
    | _ -> failwith "No such player" 
  in match_player state.players


(** [hit state] returns an updated state after dealing out a card to the player *)
let hit state = 
  let current_player = state.current_player_name in
  let rec match_player players acc = (* find current player and deal out a new card *)
    match players with 
    | h::t -> if h.name = current_player then (
        let deal_to_player = deal state.card_deck h.hand 1 in (* new deck, new player hand *)
        let new_status = if calculate_score (snd deal_to_player) > 21 then Busted else Playing in
        let players = acc@[make_player (h.name) (snd deal_to_player) new_status]@t in
        (** updated state. [current_player] is not modified.*)
        {
          players = players;
          current_player_name = current_player;
          card_deck = (fst deal_to_player);
        }
      )
      else match_player t (acc@[h])
    | _ -> failwith "No such player" 
  in match_player state.players []

let print_init_hand (state : t) : unit = print_deck_hide_first (get_hand state)

(** [check state] returns a new state with no change in player's hand*)
let check state =
  failwith "unimplemented"

(** [next_turn state] returns new state with updated [current_player_name] field to point to next player*)
let next_turn state = 
  let current_player = state.current_player_name in
  let rec next_player players = (* find current player and return next player's name *)
    match players with 
    | h::t -> if h.name = current_player then (
        if t = [] then (List.hd players).name else (List.hd t).name
      ) else next_player t
    | _ -> failwith "No such player" in
  {state with current_player_name=(next_player state.players)}

(* let check_game_status state = *)
