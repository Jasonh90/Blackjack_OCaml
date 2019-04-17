type suit = Clubs | Diamonds | Hearts | Spades

type card = {
  number: int;
  suit: suit;
}
type deck = card list

exception EmptyDeck

let rec add_cards x lst a =
  if x = a then lst else
    let club = {number = x; suit = Clubs} :: lst 
    in let heart = {number = x; suit = Hearts} :: club 
    in let diamond = {number = x; suit = Diamonds} :: heart 
    in let spade = {number = x; suit = Spades} :: diamond
    in add_cards (x-1) (spade) a

(** [shuffle lst] is a random permutation of [lst]. *)
let shuffle lst =
  QCheck.Gen.(generate1 (shuffle_l lst))

let make_deck = shuffle (add_cards 13 [] 0)

let empty_deck = []

let size (deck : deck) = List.length deck 

let rec deal deck hand num = 
  if num = 0 then (deck, hand) 
  else match deck with 
    | [] ->  raise EmptyDeck
    | h::t -> deal t (h::hand) (num-1)

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

(** [has_blackjack hand] returns boolean to indicate that the hand is blackjack *)
let has_blackjack hand = 
  calculate_score hand = 21 && size hand = 2

let poker_chip = "\xE2\x9B\x80"

let suit_style = function
  | Spades -> "\xE2\x99\xA0"
  | Clubs -> "\xE2\x99\xA3"
  | Hearts -> "\xE2\x99\xA5"
  | Diamonds -> "\xE2\x99\xA6"

let print_card1 (card : card) : unit = 
  let suit = suit_style card.suit in 
  let next_line () = ANSITerminal.(print_string [Reset] 
                                     "\n") in 
  let one_suit () = ANSITerminal.(print_string [on_black; Foreground White] 
                                    ("    " ^ suit ^ "     ")); next_line () in
  let two_suit () = ANSITerminal.(print_string [on_black; Foreground White] 
                                    ("   " ^ suit ^ " " ^ suit ^ "    ")); next_line () in
  let space () = ANSITerminal.(print_string [on_black; Foreground White] 
                                 "          "); next_line () in 
  let top_number () = 
    ANSITerminal.(print_string [on_black; Foreground White] 
                    (" " ^ string_of_int(card.number) ^ "       "^(if card.number < 10 then " " else ""))); next_line () in
  let top_num n = 
    ANSITerminal.(print_string [on_black; Foreground White] 
                    (" " ^ n ^ "        ")); next_line () in
  let bottom_number () = 
    ANSITerminal.(print_string [on_black; Foreground White] 
                    ("       "^(if card.number < 10 then " " else "") ^ string_of_int(card.number) ^ " ")); next_line (); next_line () in 
  let bot_num n = 
    ANSITerminal.(print_string [on_black; Foreground White] 
                    ("        " ^ n ^ " ")); next_line (); next_line () in 
  match card.number with 
  | 1 -> top_number ();
    for i = 0 to 4 do
      if i = 2 then one_suit ()
      else space ()
    done; bottom_number ()
  | 2 -> top_number ();
    for i = 0 to 4 do
      if i mod 2 = 1 then one_suit () else space () done; 
    bottom_number ()
  | 3 -> top_number ();
    for i = 0 to 4 do 
      if i mod 2 = 0 then one_suit () else space () done; 
    bottom_number ()
  | 4 -> top_number ();
    for i = 0 to 4 do 
      if i mod 2 = 1 then two_suit () else space () done; 
    bottom_number ()
  | 5 -> top_number (); 
    two_suit (); space (); one_suit (); space (); two_suit (); 
    bottom_number ()
  | 6 -> top_number ();
    for i = 0 to 4 do
      if i mod 2 = 0 then two_suit () else space () done;
    bottom_number ()
  | 7 -> top_number ();
    two_suit (); one_suit (); two_suit (); space (); two_suit ();
    bottom_number ()
  | 8 -> top_number ();
    for i = 0 to 4 do
      if i <> 2 then two_suit () else space () done;
    bottom_number ()
  | 9 -> top_number ();
    for i = 0 to 4 do
      if i <> 1 then two_suit () else one_suit () done;
    bottom_number ()
  | 10 -> top_number ();
    for i = 0 to 4 do
      two_suit () done;
    bottom_number ()
  | 11 -> top_num "J"; for i = 0 to 4 do if i = 0 || i = 4 then one_suit () else space () done; bot_num "J"
  | 12 -> top_num "Q"; for i = 0 to 4 do if i = 0 || i = 4 then one_suit () else space () done; bot_num "Q"
  | 13 -> top_num "K"; for i = 0 to 4 do if i = 0 || i = 4 then one_suit () else space () done; bot_num "K"
  | _ -> failwith "Wrong number"

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
                  (" " ^ string_of_int(card.number) ^ "       "^(if card.number < 10 then " " else ""))) 
let top_num n =
  ANSITerminal.(print_string [on_black; Foreground White]
                  (" " ^ n ^ "        "))
let bottom_number card = 
  ANSITerminal.(print_string [on_black; Foreground White] 
                  ("       "^(if card.number < 10 then " " else "") ^ string_of_int(card.number) ^ " "))
let bot_num n = 
  ANSITerminal.(print_string [on_black; Foreground White] 
                  ("        " ^ n ^ " "))

let hide_card (i : int) : unit = 
  let two_suit_top () = ANSITerminal.(print_string [on_black; Foreground White] 
                                        ("  " ^ suit_style Spades ^ "   " ^ suit_style Diamonds ^ "   ")) in
  let two_suit_bot () = ANSITerminal.(print_string [on_black; Foreground White] 
                                        ("  " ^ suit_style Hearts ^ "   " ^ suit_style Clubs ^ "   ")) in
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

(** [print_deck_col deck] is the [deck] shown in a column on screen. *)
let rec print_deck_col (deck : deck) : unit = 
  match deck with 
  | [] -> ()
  | h::t -> (print_card1 h); print_deck_col t

(** [print_last_card deck] is the last card in the [deck] on screen. *)
let rec print_last_card (deck:deck) : unit = 
  match deck with 
  | h::t when t = [] -> print_card1 h
  | h::t -> print_last_card t
  | _ -> ()

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
  let two_suit_top () = ANSITerminal.(print_string [on_black; Foreground White] 
                                        ("  " ^ suit_style Spades ^ "   " ^ suit_style Diamonds ^ "   ")) in
  let two_suit_bot () = ANSITerminal.(print_string [on_black; Foreground White] 
                                        ("  " ^ suit_style Hearts ^ "   " ^ suit_style Clubs ^ "   ")) in
  let middle () = ANSITerminal.(print_string [on_black; Foreground White] 
                                  ("    " ^ string_of_int(num) ^ (if num > 9 then "" else " ")  ^ "    ")) in
  match i with 
  | n when n mod 2 = 0 -> space ()
  | 1 -> two_suit_top ()
  | 3 -> middle ()
  | 5 -> two_suit_bot ()
  | _ -> failwith "i is not in between 0..5"

let show_deck_pile deck num = 
  ANSITerminal.(print_string [blue] ("\n\t Deck:\n"));
  for i = 0 to 6 do 
    space_inbtwn (); space_inbtwn (); deck_pile i num; next_line ()
  done; next_line (); 