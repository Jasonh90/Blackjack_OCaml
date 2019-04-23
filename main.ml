open Game
open Command
open State
open Ai
open Unix

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
  | Winner x -> (** either (1) dealer is the only person that won OR (2) non-dealer player(s) won *)
    print state true; 
    ANSITerminal.(print_string [blue;Bold] ("\n\nWinner(s): ")); 
    ignore(List.map (fun y -> print_string (y^" ")) x); 
    (print_string"\n\n"); exit 0;
  | Draw x -> (** multiple players won, where one of the winners is dealer *)
    print state true;ANSITerminal.(print_string [blue;Bold] ("\n\nPlayer(s) that drawed with dealer: ")); 
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

(** [before_round state] is the betting stage before the round begins. 
    Requires: [state] is initialized (init_state). *)
let rec before_round (state : State.t) prev_invalid =
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

let socket_send sock msg =
  let len = String.length msg in
  send sock (Bytes.of_string msg) 0 len []

let socket_receive client_sock  =
  let str = Bytes.create 1000 in
  let len = recv client_sock str 0 1000 []  in
  String.sub (Bytes.to_string str) 0 len

(** [play_game name] starts a new game of blackjack with player name [name]. *)
let play_game name multiplayer is_host sock =
  if not multiplayer then
    let game = init_state name in before_round game false
  else
    failwith ("unimplemented")
(* if is_host then
   let x = sock_recv sock 1000 in
   print_endline x;
   else let x = sock_send sock name in () *)

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
  | Singleplayer -> 
    let name = prompt_player_name () in
    play_game ["name"] false false None
  | Multiplayer ->
    print_endline "Enter command: 'Host' OR 'Join [Address]'\n";
    print_string  "> ";
    let sock = socket PF_INET SOCK_STREAM 0 in
    let _ = setsockopt sock SO_REUSEADDR true in (* restart server quickly *)
    match parse_socket (read_line ()) with
    | Host ->
      let host_name = gethostname() in
      let _ = bind sock (socket_addr host_name) in
      let name = prompt_player_name () in
      print_endline ("Game hosted at: "^host_name);
      print_endline ("Players in Game: ["^name^"]");
      print_endline ("Waiting for players to join...");
      let _ = listen sock 4 in (* 4 connections max *)
      let client_sock, client_addr = accept sock in
      let names = wait_for_players client_sock [name] () in
      play_game names true true sock

    | Join host_name -> 
      let _ = connect sock (socket_addr host_name) in
      print_endline ("Game joined at: "^host_name);
      let name = prompt_player_name () in
      let _ = socket_send sock name in ()

(* Execute the game engine. *)
let () = main ()
