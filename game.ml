(** The abstract type of values representing card suits. *)
type suit = Clubs | Diamonds | Hearts | Spades

(** The abstract type of values representing a single card, includes an integer 
    value and a suit of type suit. *)
type card = {
  number: int;
  suit: suit;
}

(** The abstract type of values representing a deck of cards. *)
type deck = card list

(** Raised when function is trying to perform action on an empty deck. *)
exception EmptyDeck

(** [deck_test num suit deck] add a card with number [num] and suit [suit] 
     to [deck]. This function is solely for testing. *)
let deck_test num suit deck =
  deck@[{number=num; suit=suit}]

let make_card num suit = 
  {number=num;suit=suit}

(** [add_cards x lst a] is a list where cards (a+1) through x of each suit are 
    appended to [lst] *)
let rec add_cards x lst a =
  if x = a then lst else
    let club = {number = x; suit = Clubs} :: lst 
    in let heart = {number = x; suit = Hearts} :: club 
    in let diamond = {number = x; suit = Diamonds} :: heart 
    in let spade = {number = x; suit = Spades} :: diamond
    in add_cards (x-1) (spade) a

(** [shuffle lst] is a random permutation of the deck [lst]. *)
let shuffle lst =
  QCheck.Gen.(generate1 (shuffle_l lst))

(** [empty_deck] is a deck with zero cards. *)
let empty_deck = []

(** [make_deck] creates a full deck of cards including 1-13 of each suit and 
    shuffles it *)
let make_deck = shuffle (add_cards 13 empty_deck 0)

(** [size deck] is the number of cards in [deck]. *)
let size (deck : deck) = List.length deck 

(** [deal deck hand cards_in_play num] is a tuple containing [deck] with the first 
    [num] cards removed and [hand] with the first [num] cards of [deck] appended. 
    This checks if there's enough cards in the [deck]; if there's not enough, except 
    the [cards_in_play]. The other cards will be shuffled back into the [deck], and the
    original operation takes place. *)
let rec deal (deck : deck) (hand : deck) (cards_in_play : deck)= function
  | 0 -> (deck, hand) 
  | num -> match deck with 
    | [] -> let rec new_deck acc = function
        | [] -> acc
        (* Don't include h when it's a mem of cards_in_play *)
        | h::t when List.mem h cards_in_play -> new_deck acc t
        | h::t -> new_deck (h::acc) t
      in deal (new_deck empty_deck make_deck) hand cards_in_play num
    | h::t -> deal t (h::hand) (h::cards_in_play) (num - 1)

(** [calculate_score hand] is the total value of the cards in [hand] counting cards 
    10-13 to be worth 10 points and aces to be worth either 1 or 11 points. *)
let calculate_score hand = 
  let rec total deck acc aces = 
    match deck with
    | [] -> 
      if aces = 0 then acc (* hand doens't have aces *)
      else (* If hand has aces, compute highest score possible *)
        let rec optimize_score acc one eleven best = 
          match one with 
          | -1 -> if best = -1 then acc+aces else best (* tried all combinations *)
          | _ -> let score = acc + one + (eleven*11) in 
            if score > best && score <= 21 
            then optimize_score acc (one-1) (eleven+1) score
            else optimize_score acc (one-1) (eleven+1) best
        in optimize_score acc aces 0 (-1)
    (* | [h; t] ->  *)
    | h :: t -> if h.number > 9 then total t (acc + 10) aces (* face cards *)
      else if h.number = 1 then total t acc (aces+1) (* ace card *)
      else total t (acc + h.number) aces in (* number cards *)
  total hand 0 0

(** [has_blackjack hand] is true if [hand] has a blackjack (face card & ace) *)
let has_blackjack hand = 
  calculate_score hand = 21 && size hand = 2

(** [combine_cards d1 d2] is the union of two decks [d1] & [d2]. *)
let rec combine_cards (deck1 : deck) (deck2 : deck) : deck = 
  match deck1 with
  | [] -> deck2
  | h::t -> if List.mem h deck2 
    then combine_cards t deck2 
    else combine_cards t (h::deck2)

(** [get_number card] is the int representing the number on the 
    card [card]*)
let get_number card = 
  card.number

(** [deck_to_list deck] is the card list representing [deck]*)
let deck_to_list deck : card list =
  deck

(****************************** DISPLAY CARDS ********************************)

let poker_chip = "\xE2\x9B\x80"

let suit_style = function
  | Spades -> "\xE2\x99\xA0"
  | Clubs -> "\xE2\x99\xA3"
  | Hearts -> "\xE2\x99\xA5"
  | Diamonds -> "\xE2\x99\xA6"

let suit card = suit_style card.suit 
let next_line () = ANSITerminal.(print_string [Reset] 
                                   "\n") 
let one_suit card = ANSITerminal.(print_string [on_black; Foreground White] 
                                    ("    " ^ (suit card) ^ "     "))
let two_suit card = ANSITerminal.(print_string [on_black; Foreground White] 
                                    ("   " ^ (suit card) ^ " " ^ (suit card) ^ "    ")) 
let space () = ANSITerminal.(print_string [on_black] 
                               "          ")
let space_inbtwn () = ANSITerminal.(print_string [Reset] 
                                      "   ")
let top_number card =
  ANSITerminal.(print_string [on_black; Foreground White] 
                  (" " ^ string_of_int(card.number) ^ "       "^
                   (if card.number < 10 then " " else ""))) 
let top_num n =
  ANSITerminal.(print_string [on_black; Foreground White]
                  (" " ^ n ^ "        "))
let bottom_number card = 
  ANSITerminal.(print_string [on_black; Foreground White] 
                  ("       "^(if card.number < 10 then " " else "") ^ 
                   string_of_int(card.number) ^ " "))
let bot_num n = 
  ANSITerminal.(print_string [on_black; Foreground White] 
                  ("        " ^ n ^ " "))

let hide_card (i : int) : unit = 
  let two_suit_top () = ANSITerminal.(print_string [on_black; Foreground White] 
                                        ("  " ^ suit_style Spades ^ "   " ^ 
                                         suit_style Diamonds ^ "   ")) in
  let two_suit_bot () = ANSITerminal.(print_string [on_black; Foreground White] 
                                        ("  " ^ suit_style Hearts ^ "   " ^ 
                                         suit_style Clubs ^ "   ")) in
  let middle () = ANSITerminal.(print_string [on_black; Foreground White] 
                                  ("  HIDDEN  ")) in
  match i with 
  | n when n mod 2 = 0 -> space ()
  | 1 -> two_suit_top ()
  | 3 -> middle ()
  | 5 -> two_suit_bot ()
  | _ -> failwith "i is not in between 0..5"

(** [print_card card] helps print the deck of cards side by side. 
    Requires: [i] < 7. [card] is a valid card. *)
let print_card (card : card) (i : int) : unit = 
  match i,card.number with 
  | 0,1 -> top_num "A"
  | 0,n when n < 11 -> top_number card 
  | 0,11 -> top_num "J" 
  | 0,12 -> top_num "Q" 
  | 0,13 -> top_num "K" 
  | 6,1 -> bot_num "A"
  | 6,n when n < 11 -> bottom_number card
  | 6,11 -> bot_num "J" 
  | 6,12 -> bot_num "Q" 
  | 6,13 -> bot_num "K" 
  | n,1 when n <> 3 -> space ()

  | n,2 when n mod 2 = 1 -> space () 
  | _,2  -> one_suit card
  | n,3 when n mod 2 = 0 -> space ()
  | _,3 -> one_suit card

  | n,4 when n mod 2 = 1 -> space ()
  | _,4 -> two_suit card

  | 1,5 | 1,6 | 1,7 | 3,6 | 3,7 | 5,5 | 5,6 | 5,7 | _,10 -> two_suit card
  | 2,5 | 2,6 | 4,5 | 4,6 | 4,7 | 3,8 -> space ()
  | 3,5 | 2,7 | 2,9 | 3,1 -> one_suit card

  | n,8 when n <> 3 -> two_suit card
  | n,9 when n <> 2-> two_suit card
  | m,n when n > 10 && m < 5 && m > 1-> space ()
  | _,n when n > 10 -> one_suit card

  | _ -> ()

(** [print_deck deck] is the [deck] shown side by side on screen. *)
let print_deck (deck : deck) (name : string): unit = 
  (* for i=1 to 13 do
     let deck = add_cards i [] (i-1) in *)
  for i = 0 to 6 do 
    for k = 0 to (List.length deck) - 1 do
      (print_card (List.nth deck k) i); space_inbtwn ()
    done;
    next_line ()
  done;
  next_line ()
(* done *)

(** [print_deck_hide_first deck] is the [deck] with the first card hidden on screen . *)
let print_deck_hide_first (deck : deck) (name:string): unit = 
  for i = 0 to 6 do 
    hide_card i; space_inbtwn ();
    for k = 1 to (List.length deck) - 1 do
      (print_card (List.nth deck k) i); space_inbtwn ()
    done;
    next_line ()
  done;next_line ()

let deck_pile (i : int) (num : int) : unit = 
  let two_suit_top () = ANSITerminal.(
      print_string [on_black; Foreground White] 
        ("  " ^ suit_style Spades ^ "   " ^ suit_style Diamonds ^ "   ")) in
  let two_suit_bot () = ANSITerminal.(
      print_string [on_black; Foreground White] 
        ("  " ^ suit_style Hearts ^ "   " ^ suit_style Clubs ^ "   ")) in
  let middle () = ANSITerminal.(
      print_string [on_black; Foreground White] 
        ("    " ^ string_of_int(num) ^ (if num > 9 then "" else " ")  ^ "    ")) in
  match i with 
  | n when n mod 2 = 0 -> space ()
  | 1 -> two_suit_top ()
  | 3 -> middle ()
  | 5 -> two_suit_bot ()
  | _ -> failwith "i is not in between 0..5"

(** [show_deck_pile deck num] prints on the screen the [deck] facing down with the 
    total number of cards [num] showing on top. *)
let show_deck_pile deck num = 
  ANSITerminal.(print_string [blue] ("\n\t Deck:\n"));
  for i = 0 to 6 do 
    space_inbtwn (); space_inbtwn (); deck_pile i num; next_line ()
  done; next_line ()

let string_of_suit = function 
  | Clubs -> "Clubs"
  | Diamonds -> "Diamonds"
  | Hearts -> "Hearts"
  | Spades -> "Spades"

let suit_of_string = function 
  | "Clubs" -> Clubs
  | "Diamonds" -> Diamonds
  | "Hearts" -> Hearts
  | "Spades" -> Spades
  | n -> failwith (n ^ " is invalid suit.")

let string_of_card c = string_of_int c.number ^ "#" ^ string_of_suit c.suit

let card_of_string s = 
  let string_delim = Str.split_delim(Str.regexp "#") s in 
  make_card (int_of_string (List.hd string_delim)) 
    (suit_of_string (List.nth string_delim 1))

(** [string_of_deck d] is the string representation of deck [d]. Each card 
    is separated with "@". *)
let string_of_deck d = 
  let rec convert (acc : string) = function
    | [] -> acc
    | h::t -> convert (acc ^ string_of_card h ^ (if t = [] then "" else "@")) t
  in convert "" d

(** [deck_of_string s] is the deck representation of string [s].  *)
let deck_of_string (s : string) : deck = 
  let string_delim = Str.split_delim(Str.regexp "@") s in
  let rec convert acc str = 
    match str with
    | [] -> acc
    | h::t -> convert (acc @ [card_of_string h]) t in 
  convert empty_deck string_delim
