open State
open Game

(** [dealer state] determines the moves for dealer. When dealer has cards 
    totaling to 16 or less, dealer calls hit. If not, dealer calls check. *)
let dealer state = 
  let current = get_current_player_name state in
  let hand = get_player_hand state current in
  let score = calculate_score hand in
  if score <= 17 then hit state else check state