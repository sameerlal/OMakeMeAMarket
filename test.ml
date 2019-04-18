(********************************************************************
   TESTING SUITE 
 ********************************************************************)
open OUnit2
open Trader
open Marketmaker
open Command
open Parse

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
  make_to_float_list_test "to float test 1: single elt" ["1.5"] [1.5];
]
(*Trader's types *)
let sample_bidask = {
  bid = 10;
  ask = 11;
  spread = 1
}

let sample_transaction = {
  timestamp = 1;
  bidask = sample_bidask;
  order_type = "hit"; (* bid or ask *)
}

let sample_orderbook = {
  transactions = [sample_transaction];
  buys = 1;
  sells = 0;
}

let sample_trader = {
  true_value = 125;
  avg_buy_value = 25;
  profit = 0 ;
  cash = 200;
  inventory = 4; (* Total number of shares owned *)
  orderbook = sample_orderbook;
}
let trader_tests = [

]

let suite = 
  "test suite for CamlCoin" >::: List.flatten [
    to_float_list_tests;
  ]

let _ = run_test_tt_main suite 