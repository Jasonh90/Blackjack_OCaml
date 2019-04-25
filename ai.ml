open State
open Game
open Command

(** [dealer player_lst hand] determines the moves for dealer. If all players 
    are [Busted] dealer calls check. Otherwise, when dealer has cards 
    totaling to 16 or less, dealer calls hit. If not, dealer calls check. *)
let dealer player_lst hand state = 
  if get_players_of_status player_lst Checked = [] then check state 
  else
    let score = calculate_score hand in
    if score <= 17 then hit state else check state

(** [valid_cards hand] is a list containing all the numbers that a player could 
    receive on a hit without busting *)
let valid_cards hand =
  let score = calculate_score hand in 
  let max = 21 - score in 
  let rec add_nums x lst = 
    if x > 0 then add_nums (x-1) (x :: lst)
    else lst
  in add_nums max []

(** [calc_prob x lst len] is a float containing the probability of the next 
    card having the number [x] where [len] is the number of cards left in the
    deck and [lst] gives information on the cards used already *)
let calc_prob x used : float = 
  let left = if List.mem_assoc x (get_used_cards used) 
    then 4.0 -. (float(List.assoc x (get_used_cards used)))
    else 4.0 in 
  left /. float (get_total_left used)

(** [calc_total_prob valid lst len sum] is a float containing the probability 
    of the next card having any number in the int lst [valid] where [len] is 
    the number of cards left in the deck and [lst] gives information on the
    cards used already *)
let rec calc_total_prob valid used sum : float= 
  match valid with
  |[] -> sum
  |h :: t -> calc_total_prob t used (sum +. (calc_prob h used))

(** [ai_turn used hand accuracy] calculates the probability that the next card 
    pulled will be a valid card and hits if this probability is greater than 
    [accuracy] otherwise the ai player checks *)
let ai_turn used hand accuracy state = 
  let prob = calc_total_prob (valid_cards hand) used 0.0 in 
  if prob > accuracy then hit state else check state
