open OUnit2
open Game
open State
open Command
open Ai

let make_test name var expected_output func : test =
  name >:: (fun _ -> assert_equal expected_output (func var))

let make_test2 name key typet expected_output func : test =
  name >:: (fun _ -> assert_equal expected_output (func key typet))

let make_test3 name f init typet expected_output func : test =
  name >:: (fun _ -> assert_equal expected_output (func f init typet))

let make_error_test 
    (name : string) 
    (d: string)
    (expected_output : exn) 
    (func : 'a): test = 
  name >:: (fun _ -> assert_raises expected_output (fun() -> func d))

let commandTests =
  [
    make_test "Command 1" "HIT" Hit parse;
    make_test "Command 2" "cheCk" Check parse;
    make_test "Command 3" "quiT" Quit parse;
    make_test "Command 4" "bet 4" (Bet 4) parse;
    make_error_test "Command error 1" "Bet nothing" Malformed parse;
    make_error_test "Command error 2" "" Malformed parse;
    make_error_test "Command error 3" "Error" Malformed parse;
  ]
let suite = "test suites" >::: List.flatten [
    commandTests;
  ]

let _ = run_test_tt_main suite