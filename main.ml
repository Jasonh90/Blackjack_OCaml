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

let rec play (state: State.t) (prev_invalid : bool) : unit = 
  match check_game_status state with
  | Winner x -> (** either (1) dealer is the only person that won OR (2) non-dealer player(s) won *)
    print state true; 
    ANSITerminal.(print_string [blue;Bold] ("\n\nWinner(s): ")); 
    ignore(List.map (fun y -> print_string (y^" ")) x); 
    (print_string"\n\n"); next_round state x;
  | Draw x -> (** multiple players won, where one of the winners is dealer *)
    print state true;
    ANSITerminal.(print_string [blue;Bold] ("\n\nPlayer(s) that drawed with dealer: ")); 
    ignore(List.map (fun y -> print_string (y^" ")) x); 
    (print_string"\n\n"); exit 0;
  | InProgress -> (** at least 1 player is still [Playing] status *)
    let current = State.get_current_player_name state in 
    let command = 
      if current = "Dealer" then dealer (get_players_list state) (get_player_hand state current) 
      else ( 
        print state false;
        if prev_invalid then print_invalid () else ();
        ANSITerminal.(print_string [red] ("\nIt's " ^ current ^ " turn. "));
        print_string ("Would you like to hit or check? \n> ");
        parse (read_line ()) ) in
    match command with
    | Hit -> play (hit state) false
    | Check -> play (check state) false
    | Quit -> print_quit ()
    | Bet _ -> play state true
    | exception Malformed -> play state true

(* [next_round state winners] is the transition to the next round. This checks
    who the [winners] are and correctly distributes the money to the respective players
    or the dealer.  
    Requires: [winners] does not include the dealer. 
    Returns: a new state with each player having their correct money value. *)
and next_round (state : State.t) (winners : string list) = 
  before_round (update_state (pay_up state winners)) false

(** [before_round state prev_invalid] is the betting stage before the round begins. This gets 
    the current player's info and displays it. This will ask the current player 
    how much to bet and wait for the player to respond. 
    Requires: [state] is initialized (init_state). *)
and before_round (state : State.t) (prev_invalid : bool) : unit =
  let current = get_current_player_name state in
  let current_player_wallet = get_player_wallet state current in

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

(** [play_game name] starts a new game of blackjack with player name [name]. *)
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


(* NOTES TO SELF:
1) change that hard coded part in state.ml with "jason"
2) change interface to have the current bet and different output when won or lost
3) sendingt that bool to AI.... hmm maybe part of deal in Game...?
4) Malformed isn't being detected for some reason...?
5) implement draw
6) update documentation
7)  *)