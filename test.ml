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
  make_to_float_list_test "to float test 1: single elt" ["1.5"] [1.5];
]

let suite = 
  "test suite for CamlCoin" >::: List.flatten [
    to_float_list_tests;
  ]

let _ = run_test_tt_main suite 