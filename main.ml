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

let rec play (state: State.t) (prev_invalid : bool) multiplayer is_host: unit = 
  match check_game_status state with
  | Winner x -> (** either (1) dealer is the only person that won OR (2) non-dealer player(s) won *)
    print_round_end state next_round x "Winner(s): " multiplayer is_host;
  | Draw x -> (** multiple players won, where one of the winners is dealer *)
    print_round_end state next_round x "Player(s) that drawed with dealer: " multiplayer is_host;
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
    | Hit -> play (hit state) false multiplayer is_host
    | Check -> play (check state) false multiplayer is_host
    | Quit -> print_quit ()
    | Bet _ -> play state true multiplayer is_host
    | exception Malformed -> play state true multiplayer is_host

(* [next_round state winners] is the transition to the next round. This checks
    who the [winners] are and correctly distributes the money to the respective players
    or the dealer.  
    Requires: [winners] does not include the dealer. 
    Returns: a new state with each player having their correct money value. *)
and next_round (state : State.t) (winners : string list) multiplayer is_host = 
  before_round (update_state state) false multiplayer is_host

(** [before_round state prev_invalid] is the betting stage before the round begins. This gets 
    the current player's info and displays it. This will ask the current player 
    how much to bet and wait for the player to respond. 
    Requires: [state] is initialized (init_state). *)
and before_round (state : State.t) (prev_invalid : bool) multiplayer is_host: unit =
  if not multiplayer then
    let current = get_current_player_name state in
    let current_player_wallet = get_player_wallet state current in
    if (current_player_wallet = 0) then print_string "\nYou lose! Your balance is $0!\n\n" else (
      ANSITerminal.(erase Above; 
                    if prev_invalid then (print_invalid (); print_bet_helper "Hint: bet <val>") else ();
                    print_string [white;Bold] 
                      ("\nYour current balance is: $" ^ 
                       string_of_int(current_player_wallet) 
                       ^ "\nHow much would like you like to bet?\n> "));
      match parse (read_line ()) with 
      | Bet b when b > 0 && b <= current_player_wallet -> play (bet state b) false multiplayer is_host
      | Quit -> print_quit ()
      | exception Malformed -> before_round state true multiplayer is_host
      | _ -> before_round state true multiplayer is_host
    )
  else 
    failwith "unimplemented"


let socket_send sock msg =
  let len = String.length msg in
  send sock (Bytes.of_string msg) 0 len []

let socket_receive client_sock  =
  let str = Bytes.create 1000 in
  let len = recv client_sock str 0 1000 []  in
  String.sub (Bytes.to_string str) 0 len

(** [play_game name] starts a new game of blackjack with player name [name]. *)
let play_game name has_ai multiplayer is_host sock =
  if not multiplayer then
    let game = init_state name has_ai in before_round game false false is_host
  else 
    let game = init_state name has_ai in before_round game false true is_host

let wait_for_players client_sock players call = 
  let rec concat_names lst acc= 
    match lst with 
    | h::t -> concat_names t (acc^", "^h)
    | [] -> acc in
  (* let names = concat_names (List.tl lst) (List.hd lst) in
     print_endline ("Players in Game: ["^names^"]");
     print_endline ("Waiting for players to join..."); *)
  let new_players = players@[socket_receive client_sock] in
  let names = concat_names (List.tl new_players) (List.hd new_players) in
  print_endline ("Players in Game: ["^names^"]");
  print_endline ("Starting game...") ; new_players

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
    play_game [name] has_ai false false None
  | Multiplayer ->
    print_endline "Enter command: 'Host' OR 'Join [Address]'\n";
    print_string  "> ";
    let sock = socket PF_INET SOCK_STREAM 0 in
    let _ = setsockopt sock SO_REUSEADDR true in (* restart server quickly *)
    match parse_socket (read_line ()) with
    | exception Malformed -> print_invalid ()
    | Host ->
      let host_name = gethostname() in
      let _ = bind sock (socket_addr host_name) in
      let name = prompt_player_name () in
      print_endline ("Game hosted at: "^host_name);
      print_endline ("Players in Game: ["^name^"]");
      print_endline ("Waiting for players to join...");
      let _ = listen sock 2 in (* 2 connections max *)
      let client_sock, client_addr = accept sock in
      let names = wait_for_players client_sock [name] () in
      let has_ai = prompt_ai () in
      play_game names has_ai true true sock
    | Join host_name -> 
      let _ = connect sock (socket_addr host_name) in
      print_endline ("Game joined at: "^host_name);
      let name = prompt_player_name () in
      let _ = socket_send sock name in ()


(* Execute the game engine. *)
let () = main ()


(* NOTES TO SELF:
   3) sendingt that bool to AI.... hmm maybe part of deal in Game...?
   4) Malformed isn't being detected for some reason...?
   6) update documentation
*)