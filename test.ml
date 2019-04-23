open OUnit2
open Game
open State
open Command
open Ai

let make_test name input1 expected_output func : test =
  name >:: (fun _ -> assert_equal expected_output (func input1))

let make_test2 name input1 input2 expected_output func : test =
  name >:: (fun _ -> assert_equal expected_output (func input1 input2))

let make_test3 name input1 input2 input3 expected_output func : test =
  name >:: (fun _ -> assert_equal expected_output (func input1 input2 input3))

let make_error_test 
    (name : string) 
    (d: string)
    (expected_output : exn) 
    (func : 'a): test = 
  name >:: (fun _ -> assert_raises expected_output (fun() -> func d))

let deck1 = deck_test 13 Spades empty_deck
let deck_lessThan18 = deck_test 7 Spades deck1 (* deck : 10, 7 *)
let deck_greaterThan18 = deck_test 8 Spades deck1(* deck : 10, 8 *)

let aiTests = 
  [
    make_test2 "dealer hit" [] deck_lessThan18 Hit dealer;
    make_test2 "dealer check" [] deck_greaterThan18 Check dealer;
  ]

let commandTests =
  [
    make_test "Command 1" "HIT" Hit parse;
    make_test "Command 2" "cheCk" Check parse;
    make_test "Command 3" "quiT" Quit parse;
    make_test "Command 4" "bet 4" (Bet 4) parse;
    make_error_test "Command error 1" "Bet nothing" Malformed parse;
    make_error_test "Command error 2" "" Malformed parse;
    make_error_test "Command error 3" "Error" Malformed parse;
    make_error_test "Command error 4" "hit 1" Malformed parse;
  ]

let empty = empty_deck
let blackjack = deck_test 11 Diamonds (deck_test 1 Spades empty) (* deck : ace, face *)
let deck_5 = deal make_deck blackjack empty_deck 3 (* deck : ace, face + 3 more unknown cards *)
let deck_21 = deck_test 1 Spades (deck_test 1 Hearts (deck_test 8 Diamonds (deck_test 1 Clubs empty))) (* deck : ace, ace, ace, 8 *)
let cards_in_play_shuffle = deal make_deck empty_deck empty_deck 50
let first = fst cards_in_play_shuffle
let second = snd cards_in_play_shuffle
let deck_check_shuffle = deal first empty empty_deck 5

let gameTests = 
  [
    make_test "Size 1" empty 0 size;
    make_test "Size 2" blackjack 2 size;
    make_test "Full deck" make_deck 52 size;
    make_test "Deal - new hand" (snd deck_5) 5 size;
    make_test "Deal - new deck" (fst deck_5) 49 size;
    make_test "Deal - shuffle1" (snd deck_check_shuffle) 5 size;
    make_test "Deal - shuffle2" (fst deck_check_shuffle) 47 size;
    make_test "Score - blackjack" blackjack 21 calculate_score;
    make_test "Score - 21; no blackjack" deck_21 21 calculate_score;
    make_test "Score - 17" deck_lessThan18 17 calculate_score;
    make_test "Has blackjack" blackjack true has_blackjack;
    make_test "No blackjack" deck_21 false has_blackjack;
  ]

let init = init_state "player1"

let stateTests = 
  [
    make_test "Check game status - init" init InProgress check_game_status;
  ]


let suite = "test suites" >::: List.flatten [
    commandTests;
    gameTests;
  ]

let _ = run_test_tt_main suite