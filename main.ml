open Game
open Command
open State
open Ai
open Unix

let clear_above () = ANSITerminal.erase Above

let next_line () = ANSITerminal.(print_string [Reset] "\n")

let print_invalid () = ANSITerminal.(
    print_string [on_white;red] "Invalid command. Please try again. ")

let print_bet_helper n = ANSITerminal.(print_string [on_white;magenta] n)

let print_quit () = clear_above (); ANSITerminal.(print_string [blue] ("\nGoodbye\n\n")); exit 0

let print_name name = ANSITerminal.(print_string [magenta;Bold] (name^"'s hand:\n\n"))

let print state (won : bool)= clear_above ();
  if won then print_winner state else print_dealer_hidden state

let print_round_end state f x str = 
  let updated_state = pay_up state x in
  print updated_state true;
  ANSITerminal.(print_string [blue;Bold] ("\n\n" ^ str)); 
  ignore(List.map (fun y -> print_string (y^" ")) x); 
  (print_string"\n\n"); 
  (* Write something to go to next around *)
  print_string "Press [Enter] to move to next round"; 
  ignore(read_line ()); 
  (* Move onto next round *)
  f updated_state x

let rec play (state: State.t) (prev_invalid : bool) multiplayer = 
  match check_game_status state with
  | Winner x -> (** either (1) dealer is the only person that won OR (2) non-dealer player(s) won *)
    print_round_end state next_round x "Winner(s): "; ()
  | Draw x -> (** multiple players won, where one of the winners is dealer *)
    print_round_end state next_round x "Player(s) that drawed with dealer: "; ()
  | InProgress -> (** at least 1 player is still [Playing] status *)
    let current = State.get_current_player_name state in 
    let command = 
      if current = "Dealer" then dealer (get_players_list state) (get_player_hand state current) 
      else if current = "AI" then ai_turn (get_used state) (get_player_hand state current) 0.6
      else ( 
        print state false;
        if prev_invalid then print_invalid () else ();
        ANSITerminal.(print_string [red] ("\nIt's " ^ current ^ " turn. "));
        print_string ("Would you like to hit or check? \n> ");
        parse (read_line ()) ) in
    match command with
    | Hit -> play (hit state) false multiplayer 
    | Check -> play (check state) false multiplayer 
    | Quit -> print_quit ()
    | Bet _ -> play state true multiplayer 
    | exception Malformed -> play state true multiplayer 

(* [next_round state winners] is the transition to the next round. This checks
    who the [winners] are and correctly distributes the money to the respective players
    or the dealer.  
    Requires: [winners] does not include the dealer. 
    Returns: a new state with each player having their correct money value. *)
and next_round (state : State.t) (winners : string list) multiplayer  = 
  before_round (update_state state) false multiplayer 

(** [before_round state prev_invalid] is the betting stage before the round begins. This gets 
    the current player's info and displays it. This will ask the current player 
    how much to bet and wait for the player to respond. 
    Requires: [state] is initialized (init_state). *)
and before_round (state : State.t) (prev_invalid : bool) multiplayer names: unit =
  let prompt_bet name : int = 
    let current_player_wallet = get_player_wallet state name in
    if (current_player_wallet = 0) 
    then (print_string "\nYou lose! Your balance is $0!\n\n" ; 1 )
    else (
      ANSITerminal.(erase Above; 
                    if prev_invalid then (print_invalid (); print_bet_helper "Hint: bet <val>") else ();
                    print_string [white;Bold] 
                      ("\nYour current balance is: $" ^ 
                       string_of_int(current_player_wallet) 
                       ^ "\nHow much would like you like to bet?\n> "));
      match parse (read_line ()) with 
      | Bet b when b > 0 && b <= current_player_wallet -> b
      | Quit -> print_quit ()
      | exception Malformed -> before_round state true multiplayer names; 1
      | _ -> before_round state true multiplayer names; 1
    ) in
  if not multiplayer then
    let name = List.hd names in
    let b = prompt_bet name in play (bet state b name) false multiplayer 
  else 
    let player1 = List.hd names in 
    let b1 = prompt_bet player1 in
    let state1 = bet state b1 player1 in
    let player2 = List.nth names 1 in 
    let b2 = prompt_bet player2 in
    let state2 = bet state1 b2 player2 in
    play state2 false multiplayer 

(** [play_game name] starts a new game of blackjack with player name [name]. *)
let play_game name has_ai multiplayer  =
  let game = init_state name has_ai in before_round game false multiplayer name

let prompt_player_name call = 
  print_endline "Please type your name below: \n";
  print_string  "> "; 
  let x = read_line call in
  match x with
  | exception End_of_file -> ""
  | name -> name

let prompt_ai call = 
  print_endline "Do you want an AI player to join the game? [Yes] or [No]";
  print_string  "> "; 
  let x = read_line call in
  match parse_ai x with
  | exception Malformed -> false
  | Yes -> true
  | No -> false

(** [socket_addr host_name] gives socket address for given [host_name] *)
let socket_addr host_name = 
  let inet_addr = (gethostbyname host_name).h_addr_list.(0) in
  ADDR_INET (inet_addr, 12345)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(erase Above; print_string [white;Bold]
                  "\n\nWelcome to Blackjack.\n");
  print_endline "Select game mode: [single] or [multi]\n";
  print_string  "> ";
  let x = read_line () in
  match parse_game_mode x with
  | exception Malformed -> print_invalid ()
  | Singleplayer -> 
    let name = prompt_player_name () in
    let has_ai = prompt_ai () in
    play_game [name] has_ai false 
  | Multiplayer ->
    print_endline "PLAYER 1";
    let player1 = prompt_player_name () in
    print_endline "PLAYER 2";
    let player2 = prompt_player_name () in
    let has_ai = prompt_ai () in
    play_game [player1; player2] has_ai true

(* Execute the game engine. *)
let () = main ()


(* NOTES TO SELF:
   3) sendingt that bool to AI.... hmm maybe part of deal in Game...?
   4) Malformed isn't being detected for some reason...?
   6) update documentation
*)