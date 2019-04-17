open Game
open Command
open State
open Ai

let clear_above () = ANSITerminal.erase Above


let next_line () = ANSITerminal.(print_string [Reset] "\n")
let print_invalid () = ANSITerminal.(
    print_string [on_white;red] "Invalid command. Please try again. ")
let print_bet_helper n = ANSITerminal.(
    print_string [on_white;magenta] n)
let print_quit () = clear_above(); ANSITerminal.( print_string [blue] ("\nGoodbye\n\n")); exit 0
let print_name name =
  ANSITerminal.(print_string [magenta;Bold] (name^"'s hand:\n\n"))
let print state (w:bool)= 
  clear_above ();
  if w then print_winner state else print_dealer_hidden state
  
let rec play (state: State.t) (prev_invalid : bool)= 
  match check_game_status state with
  | Winner x -> 
    print state true; 
    ANSITerminal.(print_string [blue;Bold] ("\n\nWinner(s): ")); 
    ignore(List.map (fun y -> print_string (y^" ")) x); 
    (print_string"\n\n"); exit 0;
  | End -> 
    ANSITerminal.(print_string [red;Bold] ("\n\n All players busted \n")); exit 0;
  | Playing -> 
    let current = State.get_current_player_name state in 
    if current = "Dealer" then play (dealer state) false
    else ( 
      print state false;
      if prev_invalid then print_invalid () else ();
      ANSITerminal.(print_string [red] ("\nIt's " ^ current ^ " turn. "));
      print_string ("Would you like to hit or check? \n> ");
      match parse (read_line ()) with 
      | Hit -> play (hit state) false
      | Check -> play (check state) false
      | Quit -> print_quit ()
      | Bet _ -> play state true
      | exception Malformed -> play state true
    )

(** [before_round state] is the betting stage before the round begins. 
    Requires: [state] is initialized (init_state). *)
let rec before_round (state : State.t) prev_invalid =
  let current_player_wallet = get_current_player_wallet state in

  ANSITerminal.(erase Above; 
                if prev_invalid then (print_invalid (); print_bet_helper "Hint: bet <val>") else ();
                print_string [white;Bold] 
                  ("\nYour current balance is: $" ^ 
                   string_of_int(current_player_wallet) 
                   ^ "\nHow much would like you like to bet?\n> "));
  match parse (read_line ()) with 
  | Bet b when b > 0 && b <= current_player_wallet -> play (bet state b) false
  | Quit -> print_quit ()
  | exception Malformed -> before_round state true
  | _ -> before_round state true

(** [play_game f] . *)
let play_game name =
  let game = init_state name in before_round game false

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(erase Above; print_string [white;Bold]
                  "\n\nWelcome to Blackjack.\n");
  print_endline "Please type your name below: \n";
  print_string  "> ";
  let x = read_line () in
  match x with
  | exception End_of_file -> ()
  | name -> play_game name

(* Execute the game engine. *)
let () = main ()
