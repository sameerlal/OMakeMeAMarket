(********************************************************************
   TESTING SUITE 
 ********************************************************************)
open OUnit2

let suite = "CamlCoin Test suite" >::: []

(** Tests for stats,ml  *)
open Stats

let pp_float x = string_of_float x

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

let make_to_float_list_test
    (name : string)
    (lst : string list)
    (expected_output : float list): test =
  name >:: (fun _ -> assert_equal expected_output (Stats.to_float_list lst)
  ~printer: (pp_list pp_float))

let to_float_list_tests = [
  make_to_float_list_test "to float test 1: single elt" ["1.5456"] [1.5456];
  make_to_float_list_test "to float test 2: lst" 
    ["1.5";"2"; "-1"] [1.5; 2.0; -1.0];
  make_to_float_list_test "to float test 1: empty" [] [];  
]

let make_get_mean_test
    (name : string)
    (lst : float list)
    (expected_output : float ): test =
  name >:: (fun _ -> assert_equal expected_output (Stats.get_mean lst)
  ~printer: (pp_float))

let get_mean_tests = [
  make_get_mean_test "get_mean test 1: single elt" [1.5456] 1.5456;
  make_get_mean_test "get_mean test 2: lst" 
    [1.5; 2.0; -1.0] 0.833333333333333333333333333333333333333333333;  
]

let make_get_variance_test
    (name : string)
    (lst : float list)
    (expected_output : float ): test =
  name >:: (fun _ -> assert_equal expected_output (floor (Stats.get_variance lst))
  ~printer: (pp_float))

let get_variance_tests = [
  make_get_variance_test "get_variance test 1: single elt" [1.5456] 0.0;
  make_get_variance_test "get_variance test 2: lst" 
    [1.5; 2.0; -1.0] 1.0
]

let make_last_three_lsr_test
    (name : string)
    (lst : int list)
    (expected_output : float ): test =
  name >:: (fun _ -> assert_equal expected_output (floor (Stats.last_three_lsr lst))
  ~printer: (pp_float))

let last_three_lsr_tests = [
  make_last_three_lsr_test "last_three_lsr test 2: lst" 
    [3; 2; 1] (-0.);
]

open Marketmaker
open Trader

let make_get_graph_test
    (name : string)
    (market : Marketmaker.t )
    (trader : Trader.t)
    (expected_output : graph_data ): test =
  name >:: (fun _ -> assert_equal expected_output (Stats.get_graph market trader))
  

let get_graph_tests = [
  (* make tests plz *)
]

let make_text_capture_test
    (name : string)
    (market : Marketmaker.t )
    (expected_output : unit ): test =
  name >:: (fun _ -> assert_equal expected_output (Stats.text_capture market ))

let init_market = Marketmaker.init_market "init"

let text_capture_tests = [
  make_text_capture_test "text_capture test 1" init_market ();
]

let make_linear_reg_cheat_test
    (name : string)
    (market : Marketmaker.t )
    (expected_output : float ): test =
  name >:: (fun _ -> assert_equal expected_output (Stats.linear_reg_cheat market ))
  

let linear_reg_cheat_tests = [
  (* make tests plz *)
]

let suite = 
  "test suite for CamlCoin" >::: List.flatten [
    to_float_list_tests;
    get_mean_tests;
    get_variance_tests;
    last_three_lsr_tests;
    get_graph_tests;
    text_capture_tests;
    linear_reg_cheat_tests;
  ]

let _ = run_test_tt_main suite 