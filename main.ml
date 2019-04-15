let suit_style = function
  | "Spades" -> "\xE2\x99\xA0"
  | "Clubs" -> "\xE2\x99\xA3"
  | "Hearts" -> "\xE2\x99\xA5"
  | "Diamonds" -> "\xE2\x99\xA6"
  | _ -> failwith "Wrong suit"

let card_style num s = 
  let suit = suit_style s in 
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
                    (" " ^ string_of_int(num) ^ "       "^(if num < 10 then " " else ""))); next_line () in
  let top_num n = 
    ANSITerminal.(print_string [on_black; Foreground White] 
                    (" " ^ n ^ "        ")); next_line () in
  let bottom_number () = 
    ANSITerminal.(print_string [on_black; Foreground White] 
                    ("       "^(if num < 10 then " " else "") ^ string_of_int(num) ^ " ")); next_line (); next_line () in 
  let bot_num n = 
    ANSITerminal.(print_string [on_black; Foreground White] 
                    ("        " ^ n ^ " ")); next_line (); next_line () in 
  match num with 
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


(** [play_game f] . *)
let play_game num =
  for i = 1 to 13 do card_style i "Spades";card_style i "Clubs";card_style i "Diamonds";card_style i "Hearts"done

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [white]
                  "\n\nWelcome to  Blackjack.\n");
  print_endline "Get ready for a game of Blackjack.\n";
  print_string  "> ";
  let rec x  = read_line () in
  match x with
  | exception End_of_file -> ()
  | num -> play_game (int_of_string num)

(* Execute the game engine. *)
let () = main ()
