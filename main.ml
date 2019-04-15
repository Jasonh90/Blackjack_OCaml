let rank_style = function
| 

let card_style num = 
  let next_line () = ANSITerminal.(print_string [Reset] 
                                     "\n") in
  match num with 
  | 1 -> ANSITerminal.(print_string [on_black; Foreground White] 
                         (" " ^ string_of_int(num) ^ "      "));
    next_line ();
    for i = 0 to 2 do
      if i = 1 then 
        ANSITerminal.(print_string [on_black; Foreground White] 
                        "   \xE2\x99\xA0    ")
      else
        ANSITerminal.(print_string [on_black; Foreground White] 
                        "        "); next_line ()
    done;

    ANSITerminal.(print_string [on_black; Foreground White] 
                    ("      " ^ string_of_int(num) ^ " ")); next_line ()
  | 6 -> ANSITerminal.(print_string [on_black; Foreground White] 
                         "6       "); next_line ();
    for i = 0 to 2 do
      ANSITerminal.(print_string [on_black; Foreground White] 
                      "  \xE2\x99\xA0 \xE2\x99\xA0   "); next_line ();
    done;
    ANSITerminal.(print_string [on_black; Foreground White] 
                    "      6 "); next_line ();
  | _ -> failwith "Wrong number"


(** [play_game f] . *)
let play_game num =
  card_style num

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
