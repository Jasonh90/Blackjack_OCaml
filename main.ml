open Adventure
open State


(** [play_game f] . *)
let play_game name =
  let game = init_state name in print_init_hand game

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [white]
                  "\n\nWelcome to  Blackjack.\n");
  print_endline "Please type your name below: \n";
  print_string  "> ";
  let rec x  = read_line () in
  match x with
  | exception End_of_file -> ()
  | name -> play_game name

(* Execute the game engine. *)
let () = main ()
