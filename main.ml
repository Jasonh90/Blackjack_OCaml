open Adventure
open Command
open State

let rec play (state: State.t) = 
  match check_game_status state with
  |Winner x -> ANSITerminal.(print_string [blue;Bold] ("\n\n Winner! \n")); exit 0;
  |End -> ANSITerminal.(print_string [red;Bold] ("\n\n All players busted \n")); exit 0;
  |Playing -> let current = State.get_current_player_name state in 
    if current = "dealer" then failwith "unimplemented"
    else
      ANSITerminal.(print_string [blue] ("\n\n It's your turn: " ^ current ^ "\n"));
    print_string ("\n Would you like to hit or check? \n> ");
    match parse (read_line ()) with 
    | Hit -> failwith "unimplemented"
    | Check -> failwith "unimplemented"
    | Quit -> ANSITerminal.(print_string [blue] ("\nGoodbye")); exit 0



(** [play_game f] . *)
let play_game name =
  let game = init_state name in 
  let x = print_init_hand game in 
  play game

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
