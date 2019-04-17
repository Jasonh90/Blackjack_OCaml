open Game
open Command
open State
open Ai

let print state =    print_hands state
let printw state = ANSITerminal.erase Above; print_winner state
let next_line () = ANSITerminal.(print_string [Reset] "\n\n")

let rec play (state: State.t) = 
  match check_game_status state with
  | Winner x -> 
    printw state;ANSITerminal.(print_string [blue;Bold] ("\n\nWinner(s): ")); 
    ignore(List.map (fun y -> print_string (y^" ")) x); 
    (print_string"\n\n"); exit 0;
  | Draw x -> 
    printw state;ANSITerminal.(print_string [blue;Bold] ("\n\nPlayer(s) that drawed with dealer: ")); 
    ignore(List.map (fun y -> print_string (y^" ")) x); 
    (print_string"\n\n"); exit 0;
  | Playing -> 
    let current = State.get_current_player_name state in 
    if current = "Dealer" then play (dealer state)
    else print state;
    ANSITerminal.(print_string [red] ("It's " ^ current ^ " turn:"));
    print_string (" Would you like to hit or check? \n> ");
    match parse (read_line ()) with 
    | Hit -> play (hit state)
    | Check -> play (check state)
    | Quit -> ANSITerminal.(print_string [blue] ("\nGoodbye\n\n")); exit 0
    | exception Malformed -> ANSITerminal.(
        print_string [on_white;red] "Invalid command. Please try again.");
      next_line(); 
      play state



(** [play_game f] . *)
let play_game name =
  let game = init_state name in 
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
