open OUnit2
open Adventure
open Command
open State

(********************************************************************
   Here are some helper functions for your testing of set-like lists. 
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

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

(* These tests demonstrate how to use [cmp_set_like_lists] and 
   [pp_list] to get helpful output from OUnit. *)
(* let cmp_demo = 
  [
    "order is irrelevant" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "bar"] ["bar"; "foo"]);
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "foo"] ["foo"]);
    *)
  ] *)

(********************************************************************
   End helper functions.
 ********************************************************************)

(* You are welcome to add strings containing JSON here, and use them as the
   basis for unit tests.  Or you can add .json files in this directory and
   use them, too.  Any .json files in this directory will be included
   by [make zip] as part of your CMS submission. *)

let parse_exceptions str = match parse str with
| exception Malformed -> "Malformed"
| exception Empty ->  "Empty"
| _ -> "No Exceptions"

let plaza = from_json(Yojson.Basic.from_file "ho_plaza.json")

let plaza_rooms = ["ho plaza"; "health"; "tower"; "nirvana";]
(* let blank = from_json(Yojson.Basic.from_file "blank_adv.json") *)
let ho_plaza_exits = ["southwest"; "south west"; "Cornell Health"; 
"Gannett"; "chimes"; "concert"; "clock tower";]
(* let lonely = from_json(Yojson.Basic.from_file "lonely_room.json") *)
(* let lonely_exits = [] *)

let adventure_tests =
  [
    (* TODO: add tests for the Adventure module here *)
    "Init State"  >:: 
      (fun _ -> assert_equal (start_room plaza) ("ho plaza"));

    (* "Room IDs"  >:: 
      (fun _ -> assert_equal (cmp_set_like_lists (room_ids plaza) plaza_rooms) 
      (true)); *)

    (* "Ho Plaza description" >::
    (fun _ -> assert_equal (description plaza "ho plaza") 
    ("You are on Ho Plaza. Cornell Health is to the southwest. The chimes are 
    playing a concert in the clock tower. Someone tries to hand you a 
    quartercard, but you avoid them.")); *)

    "Ho Plaza exits" >::
    (fun _ -> assert_equal (exits plaza "ho plaza")(ho_plaza_exits));

    "Ho Plaza next room" >::
    (fun _ -> assert_equal (next_room plaza "tower" "down")("ho plaza"));

    (* "lonely room init room" >::
    (fun _ -> assert_equal (start_room lonely)("the room"));
    
    "lonely room exits" >::
    (fun _ -> assert_equal (exits lonely "the room")(lonely_exits));

    "lonely room next" >::
    (fun _ -> assert_equal (cmp_set_like_lists (next_rooms lonely "the room") 
    (lonely_exits)) (true)); *)
  ]

let command_tests =
  [
    (* Finished tests for the Command module here *)
    "Empty String"  >:: 
      (fun _ -> assert_equal (parse_exceptions "") ("Empty")); 
      (* Should raise Empty Exception*)

    "Correct Go"  >:: 
      (fun _ -> assert_equal (parse "go beach") (Go ["beach";]));

    "Correct Quit"  >:: 
      (fun _ -> assert_equal (parse "quit") Quit);

    "Go then empty"  >:: 
      (fun _ -> assert_equal (parse_exceptions "go  ") ("Malformed")); 
      (* Should raise Malformed Exception*)

    "Quit then something"  >:: 
      (fun _ -> assert_equal (parse_exceptions "quit life") ("Malformed")); 
      (* Should raise Malformed Exception*)

    "Not quit not go"  >:: 
      (fun _ -> assert_equal (parse_exceptions "cool") ("Malformed")); 
      (* Should raise Malfoormed Exception*)

    "Go w/ lots space" >:: 
      (fun _ -> assert_equal (parse " go   house please  ") 
      (Go ["house"; "please"])); 
  ]
let new_state new_st = 
match new_st with 
| Legal st -> st

let firststate = init_state plaza
let newstate = new_state (go "chimes" plaza (firststate))
let newstate2 = new_state (go "higher" plaza newstate)

let state_tests =
  [
    (* TODO: add tests for the State module here *)
    "plaza init state" >::
    (fun _ -> assert_equal (current_room_id (init_state plaza)) "ho plaza");

    "visited plaza state" >::
    (fun _ -> assert_equal (visited (firststate)) ["ho plaza"]);

    "Tower state" >::
    (fun _ -> assert_equal (current_room_id newstate) "tower");

    "Higher state" >::
    (fun _ -> assert_equal (current_room_id newstate2) "nirvana");

    "Nirvana Visited plaza state" >::
    (fun _ -> assert_equal (cmp_set_like_lists (visited newstate2) ["ho plaza";
     "tower"; "nirvana";]) (true));

  ]
let suite =
  "test suite for A2"  >::: List.flatten [
    adventure_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite
