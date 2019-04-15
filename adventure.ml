

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

(** [shuffle lst] is a random permutation of [lst]. *)
let shuffle lst =
  QCheck.Gen.(generate1 (shuffle_l lst))

let make_deck = (add_cards 13 [])

let empty_deck = []

let rec deal deck hand num = 
  if num = 0 then (deck, hand) 
  else match deck with 
    | [] ->  raise EmptyDeck
    | h::t -> deal t (h::hand) (num-1)

let calculate_score hand = 
  let rec total deck acc = 
    match deck with
    |[] -> acc
    |h :: t -> total t (acc + h.number) in
  total hand 0

let suit_style = function
  | Spades -> "\xE2\x99\xA0"
  | Clubs -> "\xE2\x99\xA3"
  | Hearts -> "\xE2\x99\xA5"
  | Diamonds -> "\xE2\x99\xA6"
  | _ -> failwith "Wrong suit"

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
let space () = ANSITerminal.(print_string [on_black; Foreground White] 
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

let print_card (card : card) (i : int) : unit = 
  match i,card.number with 
  | 0,n when n < 11 -> top_number card 
  | 0,11 -> top_num "J" 
  | 0,12 -> top_num "Q" 
  | 0,13 -> top_num "K" 
  | 1,n when n = 1-> space ()
  | 2,n when n = 1 -> space ()
  | 3,n when n = 1 -> one_suit card
  | 4,n when n = 1 -> space ()
  | 5,n when n = 1 -> space ()
  | 6,n when n = 1 -> bottom_number card

  | 1,n when n = 2 -> space ()
  | 2,n when n = 2 -> one_suit card
  | 3,n when n = 2 -> space ()
  | 4,n when n = 2 -> one_suit card
  | 5,n when n = 2 -> space ()
  | 6,n when n = 2 -> bottom_number card

  | 1,n when n = 3 -> one_suit card
  | 2,n when n = 3 -> space ()
  | 3,n when n = 3 -> one_suit card
  | 4,n when n = 3 -> space ()
  | 5,n when n = 3 -> one_suit card
  | 6,n when n = 3 -> bottom_number card

  | _ -> ();
    match card.number with 
    | 1 -> 
      for i = 0 to 4 do
        if i = 2 then one_suit card
        else space ()
      done; bottom_number card
    | 2 -> 
      for i = 0 to 4 do
        if i mod 2 = 1 then one_suit card else space () done; 
      bottom_number card
    | 3 -> 
      for i = 0 to 4 do 
        if i mod 2 = 0 then one_suit card else space () done; 
      bottom_number card
    | 4 -> 
      for i = 0 to 4 do 
        if i mod 2 = 1 then two_suit card else space () done; 
      bottom_number card
    | 5 -> 
      two_suit card; space (); one_suit card; space (); two_suit card; 
      bottom_number card
    | 6 -> 
      for i = 0 to 4 do
        if i mod 2 = 0 then two_suit card else space () done;
      bottom_number card
    | 7 -> 
      two_suit card; one_suit card; two_suit card; space (); two_suit card;
      bottom_number card
    | 8 -> 
      for i = 0 to 4 do
        if i <> 2 then two_suit card else space () done;
      bottom_number card
    | 9 -> 
      for i = 0 to 4 do
        if i <> 1 then two_suit card else one_suit card done;
      bottom_number card
    | 10 -> 
      for i = 0 to 4 do
        two_suit card done;
      bottom_number card
    | 11 -> for i = 0 to 4 do if i = 0 || i = 4 then one_suit card else space () done; bot_num "J"
    | 12 -> for i = 0 to 4 do if i = 0 || i = 4 then one_suit card else space () done; bot_num "Q"
    | 13 -> for i = 0 to 4 do if i = 0 || i = 4 then one_suit card else space () done; bot_num "K"
    | _ -> failwith "Wrong number"

(** [print_deck deck] is the deck of cards side by side on screen. *)
let print_deck (deck : deck) : unit = 
  for i = 0 to 6 do 
    for k = 0 to (List.length deck) -1 do
      (print_card (List.nth deck k) i); space_inbtwn ()
    done;
    next_line ()
  done

let rec print_deck_col (deck : deck) : unit = 
  match deck with 
  | [] -> ()
  | h::t -> (print_card1 h); print_deck_col t

(** [print_last_card deck] is the deck of cards side by side on screen. *)
let rec print_last_card (deck:deck) :unit = 
  match deck with 
  | h::t when t = [] -> print_card1 h
  | h::t -> print_last_card t
  | _ -> ()