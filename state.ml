open Adventure

type player = {
  name: string;
  hand: deck;
}

type t = {
  players: player list;
  current_player_name: string;
  card_deck: deck;
}

(** [make_player str hand] makes a new player with name [str] and starting hand [hand] *)
let make_player str hand : player = 
  { name = str; hand = hand }


(** [init_state player_name] creates the initial state of the game. A new deck is
    created, two cards are handed to player with [player_name] and two cards are handed
    to the 'dealer'. The first turn goes to player.*)
let init_state player_name = 
  let deck = make_deck in (* initialize deck *)
  let deal_to_player = deal deck empty_deck 2 in (* new deck, player hand *)
  let new_deck = fst deal_to_player in
  let deal_to_dealer = deal new_deck empty_deck 2 in (* new deck, dealer hand *)
  (* create players *)
  let player = make_player player_name (snd deal_to_player) in
  let dealer = make_player "dealer" (snd deal_to_dealer) in
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


(** [new_state players current deck] returns a new state with updated [players] and [deck].
    Update [current_player_name] field of the state to point to next player's turn *)
let new_state players current deck =
  let rec next_player players = (* find current player and return next player's name *)
    match players with 
    | h::t -> if h.name = current then (
        if t = [] then (List.hd players).name else (List.hd t).name
      ) else next_player t
    | _ -> failwith "No such player" in
  (* return new state *)
  {
    players = players;
    current_player_name = next_player players;
    card_deck = deck;
  }


(** [hit state] returns a new state after dealing out a card to the player *)
let hit state = 
  let current_player = state.current_player_name in
  let rec match_player players acc = (* find current player and deal out a new card *)
    match players with 
    | h::t -> if h.name = current_player then (
        let deal_to_player = deal state.card_deck h.hand 1 in (* new deck, new player hand *)
        let players = acc@[make_player (h.name) (fst deal_to_player)]@t in
        new_state players current_player (fst deal_to_player) (** return new state *)
      )
      else match_player t (acc@[h])
    | _ -> failwith "No such player" 
  in match_player state.players []


(** [check state] returns a new state with no change in player's hand*)
let check state =
  new_state state.players state.current_player_name state.card_deck