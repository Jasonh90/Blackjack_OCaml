open Game
open Command
open State
open Ai
open Unix

(********************* Helper functions for printing *************************)
let clear_above () = ANSITerminal.erase Above

let next_line () = ANSITerminal.(print_string [Reset] "\n")

let print_invalid () = ANSITerminal.(
    print_string [on_white;red] "Invalid command. Please try again. ")

let print_bet_helper n = ANSITerminal.(print_string [on_white;magenta] n)

let print_quit () = clear_above (); 
  ANSITerminal.(print_string [blue] ("\nGoodbye\n\n")); exit 0

let print_name name = 
  ANSITerminal.(print_string [magenta;Bold] (name^"'s hand:\n\n"))

let print state (won : bool)= clear_above ();
  if won then print_winner state else print_dealer_hidden state

(* [print_round_end state ended_players msg] prints result of the round. 
   When user presses enter, function returns an updated state *)
let print_round_end updated_state ended_players msg = 
  print updated_state true;
  ANSITerminal.(print_string [blue;Bold] ("\n\n" ^ msg)); 
  ignore(List.map (fun y -> print_string (y^" ")) ended_players); 
  (print_string"\n\n"); 
  (* Write something to go to next around *)
  print_string "Press [Enter] to move to next round"; 
  ignore(read_line ())  (* Move onto next round *)

(********************* Helper functions for socket *************************)
(* [socket_send sock msg] sends a [msg] through socket *)
let socket_send sock msg =
  let len = String.length msg in
  send sock (Bytes.of_string msg) 0 len []

(* [socket_receive sock] receive a string from socket [sock] *)
let socket_receive sock  =
  let str = Bytes.create 1000 in
  let len = recv sock str 0 1000 []  in
  String.sub (Bytes.to_string str) 0 len

(** [socket_addr sock_name] gives socket address for given [sock_name] *)
let socket_addr sock_name = 
  let inet_addr = (gethostbyname sock_name).h_addr_list.(0) in
  ADDR_INET (inet_addr, 12345)

(** [extract_socket sock] parses the optional value and returns socket object *)
let extract_socket sock = 
  match sock with 
  | None -> raise Not_found
  | Some sock -> sock

(********************* Helper functions for prompts *************************)
(* [prompt_player_name call] prompts player to enter name in 
   command line and return sthe name *)
let prompt_player_name call = 
  print_endline "\nPlease type your name below: \n";
  print_string  "> "; 
  let x = read_line call in
  match x with
  | exception End_of_file -> ""
  | name -> name (* player input of name *)

(* [prompt_ai call] prompts player to enter whether or not to play with
   AI player in command line and returns a boolean that 
   corresponds to the answer *)
let rec prompt_ai call = 
  print_endline "\nDo you want an AI player to join the game? [Yes] or [No]";
  print_string  "> "; 
  let x = read_line call in
  match parse_ai x with
  | exception Malformed -> print_endline "Try Again." ; prompt_ai call;
  | Yes -> true
  | No -> false

(* [prompt_bet state prev_invalid name] prompts player to bet. Returns an 
   integer indication how much player wants to bet.*)
let rec prompt_bet state prev_invalid name =
  let player_wallet = get_player_wallet state name in
  if player_wallet = 0
  then (print_string "\nYou lose! Your balance is $0!\n\n" ; 0)
  else (
    ANSITerminal.
      (erase Above; 
       if prev_invalid 
       then (print_invalid (); print_bet_helper "Hint: bet <val>") else ();
       print_string [white;Bold] 
         ("\nYour current balance is: $" ^ 
          string_of_int(player_wallet) 
          ^ "\nHow much would like you like to bet?\n> "));
    match parse (read_line ()) with 
    | Bet b when b > 0 && b <= player_wallet -> b 
    | Quit -> print_quit ()
    | exception Malformed -> prompt_bet state true name
    | _ -> prompt_bet state true name 
  ) 

(* [prompt_command state prev_invalid name] prompts player to type a command
   to progress the game. Performs necessary actions corresponding to
   the command and returns the updated state. *)
let rec prompt_command state prev_invalid name =
  print state false;
  if prev_invalid then print_invalid () else ();
  ANSITerminal.(print_string [red] ("\nIt's " ^ name ^ " turn. "));
  print_string ("Would you like to hit or check? \n> ");
  match parse (read_line ()) with
  | Hit -> hit state
  | Check -> check state
  | Quit -> print_quit ()
  | Bet _ -> prompt_command state true name
  | exception Malformed -> prompt_command state true name

(********************* Helper functions for game play ************************)
(* [play state host_name client_sock names_lst multiplayer] handles how to 
   progress the game depending on game_status *)
let rec play state host_name client_sock names_lst multiplayer has_ai = 
  match check_game_status state with
  (** either (1) dealer is the only person that won OR 
      (2) non-dealer player(s) won *)
  | Winner end_lst -> 
    let updated_state = pay_up state end_lst in
    let _ = if multiplayer then ( (** send result to other player *)
        let extracted_client_sock = extract_socket client_sock in
        let _ = socket_send extracted_client_sock 
            ("WIN "^(string_of_state updated_state)) in ()
      ) else () in
    let _ = print_round_end updated_state end_lst "Winner(s): " in
    next_round updated_state names_lst multiplayer client_sock has_ai
  (** multiple players won, where one of the winners is dealer *)
  | Draw end_lst -> 
    let updated_state = pay_up state end_lst in
    let _ = if multiplayer then ( (** send result to other player *)
        let extracted_client_sock = extract_socket client_sock in
        let _ = socket_send extracted_client_sock 
            ("DRAW "^(string_of_state updated_state)) in ()
      ) else () in
    let _ = print_round_end updated_state end_lst 
        "Player(s) that drawed with dealer: " in
    next_round updated_state names_lst multiplayer client_sock has_ai
  (** at least 1 player is still [Playing] status *)
  | InProgress -> 
    let current = State.get_current_player_name state in 
    let new_state = 
      if current = "Dealer" then
        dealer (get_players_list state) (get_player_hand state current) state 
      else if current = "AI" then 
        ai_turn (get_used state) (get_player_hand state current) 0.6 state
      else if current = host_name then
        prompt_command state false current
      else (
        print_endline "\nWaiting on another player...";
        (** send state to other player to make a move; 
            receives the modified state  *)
        let extracted_client_sock = extract_socket client_sock in
        let _ = socket_send extracted_client_sock 
            ("PLAY "^(string_of_state state)) in
        state_of_string (socket_receive extracted_client_sock)
      )
    in play new_state host_name client_sock names_lst multiplayer has_ai

(* [before_round state names_lst multiplayer client_sock] is the betting stage
   before the round begins.*)
and before_round state names_lst multiplayer client_sock has_ai = 
  let host_name = List.hd names_lst in
  let host_bet_val = prompt_bet state false host_name in
  let new_state = 
    let temp_state = bet state host_bet_val host_name in
    (* handle AI player betting *)
    let temp_state = 
      if has_ai then bet state (ai_bet state) "AI" else temp_state in
    (* handle multiplayer betting *)
    if not multiplayer then temp_state
    else (
      print_endline "\nWaiting on another player...";
      (* send socket to client for betting input; receive updated 
         state after member bets *)
      let extracted_client_sock = extract_socket client_sock in
      let _ = socket_send extracted_client_sock 
          ("BET "^(string_of_state temp_state)) in 
      state_of_string (socket_receive extracted_client_sock) 
    ) in
  play new_state host_name client_sock names_lst multiplayer has_ai

(* [next_round state names_lst multiplayer client_sock] is the transition to 
   the next round. This checks who the [winners] are and correctly 
   distributes the money to the respective players or the dealer. *)
and next_round state names_lst multiplayer client_sock has_ai = 
  before_round (update_state state) names_lst multiplayer client_sock has_ai

(* [start_game names has_ai multiplayer client_sock] initializes a 
   new game state and starts the game with betting stage *)
let start_game names has_ai multiplayer client_sock = 
  let start_state = init_state names has_ai in (* initialize game *)
  before_round start_state names multiplayer client_sock has_ai

(********************* Functions for starting a game *************************)
(* [wait_for_players client_sock players] listens to client socket for 
   second player's name and return a complete list of human players' names *)
let wait_for_players client_sock players = 
  let extracted_client_sock = extract_socket client_sock in
  (* [concat_names lst acc] return string of format:
     "player1name, player2name" *)
  let rec concat_names lst acc = 
    match lst with 
    | h::t -> concat_names t (acc^", "^h)
    | [] -> acc in
  let new_players = 
    players@[socket_receive extracted_client_sock] in (*list of players' names*)
  let names = concat_names (List.tl new_players) (List.hd new_players) in
  print_endline ("Players in Game: ["^names^"]");
  print_endline ("Starting game...") ; 
  new_players

(** [main ()] prompts for the game to play, then starts it. *)
let main () : unit =
  ANSITerminal.(erase Above; print_string [white;Bold]
                  "\n\nWelcome to Blackjack.\n");
  (* select player mode *)
  print_endline "\nSelect game mode: [single] or [multi]\n"; 
  print_string  "> ";
  let x = read_line () in
  match parse_game_mode x with
  | exception Malformed -> print_invalid ()
  | Singleplayer -> 
    let name = prompt_player_name () in
    let has_ai = prompt_ai () in
    start_game [name] has_ai false None
  | Multiplayer ->
    let host_sock = socket PF_INET SOCK_STREAM 0 in (* create socket *)
    let _ = setsockopt host_sock SO_REUSEADDR true in (* restart server quickly *)
    (* select whether to host or join a game *)
    print_endline "\nEnter command: 'Host' OR 'Join [Address]'\n";
    print_string  "> ";
    match parse_socket (read_line ()) with
    | exception Malformed -> print_invalid ()
    | Host ->
      let sock_name = gethostname() in (* get socket name *)
      let _ = bind host_sock (socket_addr sock_name) in (* bind socket *)
      let host_name = prompt_player_name () in (* get host player's name *)
      print_endline ("\nGame hosted at: "^sock_name);
      print_endline ("Players in Game: ["^host_name^"]");
      print_endline ("Waiting for players to join...");
      let _ = listen host_sock 2 in (* 2 connections max *)
      let client_sock, client_addr = accept host_sock in 
      (* ^^ accept client connection *)
      let names = wait_for_players (Some client_sock) [host_name] in 
      (* ^^ complete list of human players' names *)
      let has_ai = prompt_ai () in
      start_game names has_ai true (Some client_sock)
    | Join sock_name -> 
      let _ = connect host_sock (socket_addr sock_name) in 
      (* ^^ connect to host socket *)
      print_endline ("Game joined at: "^sock_name);
      let member_name = prompt_player_name () in
      (* ^^ get member player's name *)
      print_endline "\nWaiting on another player...";
      let _ = socket_send host_sock member_name in
      (* ^^send member name to host socket *)
      (* [listen_to_host host] makes sure that client socket is constantly
         listening to the host socket. Processes messages received from 
         host socket *)
      let rec listen_to_host host = 
        (* ^^ constantly listen to messages from host socket *)
        let info = socket_receive host in 
        match Str.split (Str.regexp " ") info with 
        | ["BET"; state_str] -> 
          let state = state_of_string state_str in
          let bet_val = prompt_bet state false member_name in 
          (* ^^ prompt member to bet *)
          let new_state = bet state bet_val member_name in 
          let _ = socket_send host_sock (string_of_state new_state) in
          (* ^^ send updated state to host *)
          print_endline "\nWaiting on another player...";
          listen_to_host host_sock
        | ["PLAY"; state_str] ->
          let state = state_of_string state_str in
          let new_state = prompt_command state false member_name in 
          (* ^^ prompt member to input a command *)
          let _ = socket_send host_sock (string_of_state new_state) in
          (* ^^ send updated state to host *)
          listen_to_host host_sock
        | ["WIN"; state_str] ->(
            let state = state_of_string state_str in
            match check_game_status state with
            | Winner end_lst ->
              let _ = print_round_end state end_lst "Winner(s): " in 
              (* ^^ print end results *)
              print_endline "\nWaiting on another player...";
              listen_to_host host_sock
            | _ -> ())
        | ["DRAW"; state_str] -> (
            let state = state_of_string state_str in
            match check_game_status state with
            | Draw end_lst ->
              let _ = print_round_end state end_lst 
                  "Player(s) that drawed with dealer: " in (* print end results *)
              print_endline "\nWaiting on another player...";
              listen_to_host host_sock
            | _ -> ())
        | _ -> listen_to_host host in
      listen_to_host host_sock

(* Execute the game engine. *)
let () = main ()