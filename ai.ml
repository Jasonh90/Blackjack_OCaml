open State
open Game
open Command

(** [dealer player_lst hand] determines the moves for dealer. If all players are [Busted] 
    dealer calls check. Otherwise, when dealer has cards otaling to 16 or less, 
    dealer calls hit. If not, dealer calls check. *)
let dealer player_lst hand = 
  if get_players_of_status player_lst Checked = [] then Check 
  else
    let score = calculate_score hand in
    if score <= 17 then Hit else Check