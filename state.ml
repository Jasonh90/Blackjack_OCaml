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

let init_state player_name = 
  let deck = make_deck in (* initialize deck *)
  let deal_to_player = deal deck [] 2 in (* new deck, player hand *)
  let new_deck = fst deal_to_player in
  let deal_to_dealer = deal new_deck [] 2 in (* new deck, dealer hand *)

  (* create players *)
  let player = make_player player_name (snd deal_to_player) in
  let dealer = make_player "dealer" (snd deal_to_dealer) in

  (* return initialized state *)
  {
    players = [player; dealer];
    current_player_name = player.name;
    card_deck = fst deal_to_dealer;
  }

let get_hand state = 
  let current_player = state.current_player_name in
  let rec match_player players = (* find current player and return player's hand *)
    match players with 
    | h::t -> if h.name = current_player then h.hand else match_player t
    | _ -> failwith "No such player" 
  in match_player state.players

let hit state = 
  let current_player = state.current_player_name in
  let rec match_player players acc = (* find current player and modify hand *)
    match players with 
    | h::t -> if h.name = current_player then (
        let deal_to_player = deal state.card_deck h.hand 1 in (* new deck, new player hand *)
        let next_player = if t = [] then (List.hd acc).name else (List.hd t).name in
        {
          players = acc@[make_player (h.name) (fst deal_to_player)]@t;
          current_player_name = next_player;
          card_deck = (fst deal_to_player);
        }
      )
      else match_player t (acc@[h])
    | _ -> failwith "No such player" 
  in match_player state.players []

let check state = failwith "Unimplemented"