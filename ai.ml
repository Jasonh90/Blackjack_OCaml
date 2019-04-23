open State
open Game
open Command

(** [dealer player_lst hand] determines the moves for dealer. If all players are [Busted] 
    dealer calls check. Otherwise, when dealer has cards totaling to 16 or less, 
    dealer calls hit. If not, dealer calls check. *)
let dealer player_lst hand = 
  if get_players_of_status player_lst Checked = [] then Check 
  else
    let score = calculate_score hand in
    if score <= 17 then Hit else Check

type used_deck = {
  used_cards: (int * int) list;
  total_left: int;
}

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
    add_used_cards {used with used_cards= (add_deck old_used_cards (Game.deck_to_list (get_hand h)))} t

(** [valid_cards ] is a list containing all the numbers that a player could receive
    on a hit without busting*)
let valid_cards hand =
  let score = calculate_score hand in 
  let max = 21 - score in 
  let rec add_nums x lst = 
    if x > 0 then add_nums (x-1) (x :: lst)
    else lst
  in add_nums max []

(** [calc_prob x lst len] is a float containing the probability of the next card 
    having the number [x] where [len] is the number of cards left in the deck and 
    [lst] gives information on the cards used already*)
let calc_prob x used : float = 
  let left = if List.mem_assoc x used.used_cards then 4.0 -. (float(List.assoc x used.used_cards))
    else 4.0 in 
  left /. float(used.total_left)

(** [calc_total_prob valid lst len sum] is a float containing the probability of the next card 
    having any number in the int lst [valid] where [len] is the number of cards left in the deck and 
    [lst] gives information on the cards used already*)
let rec calc_total_prob valid used sum : float= 
  match valid with
  |[] -> sum
  |h :: t -> calc_total_prob t used (sum +. (calc_prob h used))

(** [ai_turn used hand accuracy] calculates the probability that the next card 
    pulled will be a valid card and hits if this probability is greater than 
    [accuracy] otherwise the ai player checks*)
let ai_turn used hand accuracy= 
  let prob = calc_total_prob (valid_cards hand) used 0.0 in 
  if prob > accuracy then Hit else Check

(** [restart] is a new used_deck with no bindings to be used 
    when the deck is reshuffled*)
let restart () : used_deck = {used_cards = []; total_left = 52;}