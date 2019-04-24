(********************************************************************
   TESTING SUITE 

   Individual Tests are organized as follows
   - Normal Tests
   - Smoke Tests
   - Edge Cases
 ********************************************************************)
open OUnit2
open Yojson.Basic.Util
open Trader
open Marketmaker
open Command
open Parse
open Stats 

let suite = "CamlCoin Test suite" >::: []

(** Tests for stats,ml  *)

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
    (dice : Stats.dice_data)
    (expected_output : float ): test =
  name >:: (fun _ -> assert_equal expected_output (Stats.linear_reg_cheat market dice ))


let linear_reg_cheat_tests = [
  (* make tests plz *)
]

let make_chebyshevs_var_test
    (name : string)
    (var : float)
    (expected_output : float ): test =
  name >:: (fun _ -> assert_equal expected_output (floor (Stats.chebyshevs_var var))
               ~printer: (pp_float))

let chebyshevs_var_tests = [
  make_chebyshevs_var_test "chebyshevs_var test 1: 0" 0. 0.;
  make_chebyshevs_var_test "chebyshevs_var test 2: #" 42. 0.
]


(*Trader's tests stuff *)
let sample_bidask = {
  bid = 10;
  ask = 1100;
  spread = 1090
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
  id = "1";
  hidden_number = 125;
  avg_buy_value = 0;
  profit = 0 ;
  cash = 200;
  inventory = 2; (* Total number of shares owned *)
  orderbook = sample_orderbook;
}
let test1_bidask = {
  bid = 42;
  ask = 4242;
  spread = 4200
}
let test1_transaction = {
  timestamp = 2;
  bidask = test1_bidask;
  order_type = "lift"
}
let trader_tests = [
  "init_trader" >:: (fun _ -> assert_equal ({id = "1"; hidden_number = 20; avg_buy_value = 0; profit = 0; cash = 1000000; inventory = 0; 
                                             orderbook = {transactions = []; buys = 0; sells = 0}}) (init_trader 20 "1")) ;
  "make_trade_test" >:: (fun _ -> assert_equal (None) (make_trade sample_trader sample_transaction));
  "make_trade_dumb_test 1" >:: (fun _ -> assert_equal (None) (make_trade_dumb sample_trader sample_transaction));
  "make_transaction test 1" >:: (fun _-> assert_equal test1_transaction (make_transaction 2 42 4242 "lift"))
]
(*Marketmaker's tests stuff*)
let samplem_bidask = {
  trade_type = "init";
  bid=10;
  ask=1100;
  spread=1090;
}

(* keeps track of market maker's holdings *)
let samplem_orderbook = {
  outstanding_shares=0;
}

(* Holds market maker type *)
let sample_market = {
  currbidask=samplem_bidask;
  bid_ask_history=[samplem_bidask];
  timestamp=0;
  curr_profit=0;
  orderbook=samplem_orderbook
}

let sample_market_2 = {
  currbidask={samplem_bidask with trade_type = "hit"};
  bid_ask_history=[samplem_bidask; {samplem_bidask with trade_type = "hit"}];
  timestamp=1;
  curr_profit= -10;
  orderbook={outstanding_shares=1}
}
(* Trade variant.  To be sent to engine *)

let sample_send_market = {
  timestamp = 0;
  transaction = samplem_bidask;
}

let sample_receive_transaction =  {
  timestamp=0;
  trade_type="hit"; (* hit the bid or lifted offer *)
  transaction=samplem_bidask;
}
let marketmaker_tests = [
  "init_trader_test" >:: (fun _ -> assert_equal ({
      currbidask = {
        trade_type = "init";
        bid = 0;
        ask = 0;
        spread = 0;
      };
      bid_ask_history = [];
      timestamp = 0;
      curr_profit = 0;
      orderbook = {
        outstanding_shares = 0
      }
    }) (Marketmaker.init_market "init")) ;
  "transaction_test" >:: 
  (fun _ -> assert_equal (sample_market_2) 
      (transaction sample_receive_transaction sample_market));

]

(* Tests for parse.ml *)

let fermi = Yojson.Basic.from_file "fermi.json"
let fermi_json = Parse.fermi_json

let make_from_json_test
    (name : string)
    (market : Yojson.Basic.t )
    (expected_output : Parse.t ): test =
  name >:: (fun _ -> assert_equal expected_output (Parse.from_json market ))


let from_json_tests = [
  make_from_json_test "from_json test 1: fermi" fermi fermi_json
]

let pp_string x = x 
let pp_int x = string_of_int x
let make_get_question_test
    (name : string)
    (data : Parse.t )
    (expected_output : int ): test =
  name >:: (fun _ -> assert_equal expected_output (Parse.get_question data |> String.length)
  ~printer: pp_int)


let get_question_tests = [
  make_get_question_test "get_question test 1: fermi" fermi_json 109
]

let make_get_answer_test
    (name : string)
    (data : Parse.t )
    (expected_output : string ): test =
  name >:: (fun _ -> assert_equal expected_output (Parse.get_answer data )
               ~printer: pp_string)


let get_answer_tests = [
  make_get_answer_test "get_answer test 1: fermi" fermi_json "125"
]

let oth_situation = Parse.oth_situation
let first_situation = Parse.first_situation

let make_get_nth_situation_test
    (name : string)
    (index: int)
    (data : Parse.t )
    (expected_output : Parse.situation ): test =
  name >:: (fun _ -> assert_equal expected_output 
               (Parse.get_nth_situation index data))


let get_nth_situation_tests = [
  make_get_nth_situation_test "get_nth_situation test 1: 0th" 0 fermi_json oth_situation;
  make_get_nth_situation_test "get_nth_situation test 1: 1st" 1 fermi_json first_situation; 
]

let make_get_event_from_situation_test
    (name : string)
    (sit : Parse.situation )
    (expected_output : string ): test =
  name >:: (fun _ -> assert_equal expected_output 
               (Parse.get_event_from_situation sit)
               ~printer: pp_string)


let get_event_from_situation_tests = [
  make_get_event_from_situation_test "get_event_from_situation test 1: 0th" oth_situation "Flash Crash, all stocks are down";
  make_get_event_from_situation_test "get_event_from_situation test 1: 1st" first_situation "Interest rates decrease"; 
]

let make_get_effect_from_situation_test
    (name : string)
    (sit : Parse.situation )
    (expected_output : (string * string) option ): test =
  name >:: (fun _ -> assert_equal expected_output 
               (Parse.get_effect_from_situation sit))


let get_effect_from_situation_tests = [
  make_get_effect_from_situation_test "get_effect_from_situation test 1: 0th" oth_situation (Some ("decrease", "10%"));
  make_get_effect_from_situation_test "get_effect_from_situation test 1: 1st" first_situation (Some ("increase", "2%")); 
]

let make_get_intro_test
    (name : string)
    (data : Parse.t )
    (expected_output : unit ): test =
  name >:: (fun _ -> assert_equal expected_output 
               (Parse.get_intro data))


let get_intro_tests = [
  make_get_intro_test "get_intro test 1: fermi" fermi_json ();
]

(* Command.ml tests *)
let make_parse_test
    (name : string)
    (str : string )
    (expected_output : Command.command ): test =
  name >:: (fun _ -> assert_equal expected_output (Command.parse str))


let parse_tests = [
  make_parse_test "Parse test 1: set" "set 10 20" (Set (["10";"20"]));
  make_parse_test "Parse test 2: inventory" "inventory" Inventory;
  make_parse_test "Parse test 3: history" "history" History;
  make_parse_test "Parse test 4: quit" "quit" Quit;
  make_parse_test "Parse test 5: tutorial" "tutorial" Tutorial;
  make_parse_test "Parse test 6: help" "help" Help;
  make_parse_test "Parse test 7: cheat" "cheat" Cheat;
]


let suite = 
  "test suite for CamlCoin" >::: List.flatten [
    to_float_list_tests;
    get_mean_tests;
    get_variance_tests;
    last_three_lsr_tests;
    get_graph_tests;
    (* text_capture_tests; *)
    linear_reg_cheat_tests;
    (* from_json_tests; *)
    get_question_tests;
    get_answer_tests;
    get_nth_situation_tests;
    get_event_from_situation_tests;
    get_effect_from_situation_tests;
    (* get_intro_tests; *)
    trader_tests;
    marketmaker_tests;
    parse_tests;
  ]

let _ = run_test_tt_main suite 