

type suit = Clubs | Diamonds | Hearts | Spades

type card = {
  number: int;
  suit: suit;
}
type deck = card list

exception EmptyDeck

let rec add_cards x lst =
  if x = 0 then lst else
    let club = {number = x; suit = Clubs} :: lst 
    in let heart = {number = x; suit = Hearts} :: club 
    in let diamond = {number = x; suit = Diamonds} :: heart 
    in let spade = {number = x; suit = Spades} :: diamond
    in add_cards (x-1) (spade)

let make_deck = add_cards 13 []

let shuffle = 
  failwith "unimplemented"


let rec deal deck hand num = 
  if num = 0 then (deck, hand) 
  else match deck with 
    | [] ->  raise EmptyDeck
    | h::t -> deal t (h::hand) (num-1)

let rec total acc = function
  |[] -> acc
  |h :: t -> total (acc + h.number) t

(** [shuffle lst] is a random permutation of [lst]. *)
let shuffle lst =
  QCheck.Gen.(generate1 (shuffle_l lst))