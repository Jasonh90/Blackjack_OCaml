(** [play_game f] starts the adventure in file [f]. *)
let play_game f =
  failwith "Unimplemented"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [white]
                  "\n\nWelcome to  Blackjack.\n");
  print_endline "Would you like to be first or second?.\n";
  print_string  "> ";
  while (read_line ()) <> "1" || (read_line () <> "2") do
    match read_line () with
    | exception End_of_file -> ()
    | _ -> play_game ()
  done

(* Execute the game engine. *)
let () = main ()
