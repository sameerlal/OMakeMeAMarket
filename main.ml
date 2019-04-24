(* 
 *   Main Engine - Beta
 *
 *
*)

open Yojson.Basic.Util
open Marketmaker
open Trader
open Command
open Parse
open Gui

(**  [opponents] denotes the number of opposing traders. *)
let opponents = 3

exception UnknownFile

type trader_players = Trader.trader_players

(** [big_state] record holds the states of the market maker,
    other traders and information regarding the dice roll. *)
type big_state = {
  mmstate : Marketmaker.t;
  traders : trader_players;
  dice : Stats.dice_data
}

(**[init_trader_players roll_list] is the group of traders playing with the 
   player. *)
let init_trader_players (rolls:int list) : trader_players = 
  { simple_ai = (Trader.init_trader (List.nth rolls 0) "1");
    ai1 = (Trader.init_trader (List.nth rolls 1) "2");
    ai2 = (Trader.init_trader (List.nth rolls 2) "3");
  }  


(**[parse_user_input state] parses the user's input and returns a Command 
   variant classifying the response. If the user does not input anything or 
   inputs a malformed input, it will reprompt the user to answer until the user 
   has provided correct input. *)
let rec parse_user_input state = 
  print_endline "\n \n Make a market:   \n";
  print_string "> ";
  match read_line() with
  | exception End_of_file -> print_endline "Try again . . ."; 
    parse_user_input state
  | user_response -> begin
      try Command.parse user_response with
      | Command.Empty -> parse_user_input state 	
      | Command.Malformed -> print_endline "Error in command."; 
        parse_user_input state  
    end

(**[cycle_traders bstate] returns unit and is used to print out the hidden 
   numbers of traders and the sum (intrinsic value).  This is only used in 
   debug mode.  *)
let cycle_traders (bstate:big_state) : unit = 
  let tr = bstate.traders in
  print_endline "hidden numbers; ";
  print_endline (string_of_int tr.simple_ai.hidden_number);
  print_endline (string_of_int tr.ai1.hidden_number);
  print_endline (string_of_int tr.ai2.hidden_number);
  print_endline "Player";
  print_endline (string_of_int bstate.dice.player_roll);
  print_endline "Sum: ";
  print_endline (string_of_int bstate.dice.sum_rolls)


(**[fsm fermi state] is the looping fsm that keeps the game playing based on 
   the commands put in by the user.  It takes in [state] and then returns a 
   new state according to a transaction that occurs.  In the case that no 
   transaction occurs, it increments the timestep and returns the old state. 
   It will throw errors with descriptions if unintended behavior occurs 
   (for debug mode).
*)
let fsm fermi (state: big_state) = 
  let user_command = parse_user_input state in 
  if (Marketmaker.get_timestamp state.mmstate) = 69 then 
    (* TODO IN NEXT PHASE:  CHANGE STATE WITH NEW EVENTS *)
    state 
  else 
    match user_command with 
    | Command.Quit -> print_endline "QUIT"; exit 0
    | Command.Inventory -> (Marketmaker.display_data state.mmstate); state
    | Command.History -> (Marketmaker.stringify_bidask_history state.mmstate); 
      state
    | Command.Help -> Gui.display_help (); state
    | Command.Tutorial -> Gui.tutorial_preamble "start"; state
    | Command.Cheat -> 
      print_endline( (
          if Stats.linear_reg_cheat state.mmstate
             = infinity || Stats.linear_reg_cheat state.mmstate = -1.0
          then "No cheat available! "
          else 
            string_of_float (Stats.linear_reg_cheat state.mmstate)
        ));
      state
    | Command.Set phr -> 
      let bid = int_of_string (List.nth phr 0) in 
      let ask = int_of_string (List.nth phr 1) in 
      let trade_transaction = Trader.make_transaction 
          (Marketmaker.get_timestamp state.mmstate) bid ask "blank" in
      let trader_response = Trader.contention_for_trade state.traders 
          trade_transaction in 

      begin
        match trader_response with 
        | None -> print_endline "No Trade occured"; 
          {
            state with 
            mmstate = (Marketmaker.increment_timestep state.mmstate)
          }
        | Some (new_trader_state, response) -> 
          let new_MM_state = Marketmaker.transaction 
              (Marketmaker.generate_receive_transaction 
                 (Marketmaker.get_timestamp state.mmstate) 
                 response bid ask) state.mmstate in

          let new_state = begin 
            match (new_trader_state.id) with
            | "1" -> {
                mmstate = new_MM_state; 
                traders = {
                  state.traders with
                  simple_ai = new_trader_state
                };
                dice = state.dice
              }
            | "2" -> {
                mmstate = new_MM_state; 
                traders = {
                  state.traders with
                  ai1 = new_trader_state
                };
                dice = state.dice
              }
            | "3" -> {
                mmstate = new_MM_state; 
                traders = {
                  state.traders with
                  ai2 = new_trader_state
                };
                dice = state.dice
              }  
            | _ -> failwith "trader range out of bounds"
          end
          in 
          match response with
          | "lift" -> print_endline ("Trader " ^ new_trader_state.id ^ 
                                     " bought a CamlCoin (lifted your offer)"); 
            new_state
          | "hit" -> print_endline ("Trader " ^ new_trader_state.id ^ 
                                    " sold a CamlCoin (hit your bid)"); 
            new_state
          | _ -> print_endline "error in trader's make trade"; exit 2
      end



(**[cli fermi big_state] will pretty-print the market maker's state as it 
   appears in [big_state] as well as print information from the [fermi] json 
   file that is relevant to the game.  [cli] is recursive in nature and is 
   hardcoded to stop the game after ten increments.  This number can be varied 
   without affect other parts of the game. *)
let rec cli fermi big_state = 
  ANSITerminal.(print_string [blue]
                  "------------------- Statistics ------------------- \n");
  (* cycle_traders big_state; *)
  print_string ("\nTimestamp ");
  print_string ( string_of_int 
                   (Marketmaker.get_timestamp big_state.mmstate + 1));
  print_string (" |  Prev. bid/ask:  ");
  print_string (Marketmaker.stringify_bid_ask big_state.mmstate);
  print_string (" |  CamlCoins Accumulated: " );
  print_string (string_of_int (Marketmaker.get_outstandingshares 
                                 big_state.mmstate));
  match (Marketmaker.get_timestamp big_state.mmstate) with
  | 10 -> print_endline ("\n GAME OVER \n \n ");
    (Marketmaker.display_data big_state.mmstate);
    ANSITerminal.print_string [ANSITerminal.blue] 
      " Cashing in for true value:  ";
    print_endline 
      ("1 CamlCoin =  $" ^ (string_of_int big_state.dice.sum_rolls) ^ ".");
    let final_mm_state = (Marketmaker.exchange_mm_excess big_state.mmstate 
                            big_state.dice.sum_rolls) in 
    (Marketmaker.display_data final_mm_state);
    exit 0
  | _ ->  cli fermi (fsm fermi big_state )


(**[init_big_state true_value] returns an initial big_state with all record 
   fields initialized, where [num_opponents] denotes the number of opposing 
   traders.  While this is not used in computation, it is included for debug 
   help. *)
let init_big_state num_opponents = 
  let dice_state = Stats.mega_roll opponents in
  {
    mmstate =  Marketmaker.init_market "init";
    traders = init_trader_players dice_state.other_rolls;
    dice = dice_state;
  }


(** [play_game f] initiates the start of the game described by json file [f]. 
    It outputs a description of the starting game state as well as a nice 
    visual for the dice roll. *)
let play_game f =
  let obtained_json = Yojson.Basic.from_file f in
  let fermi = Parse.from_json obtained_json in
  let big_state = init_big_state opponents in 
  ANSITerminal.erase Above;
  Parse.get_intro fermi;
  Unix.sleep 3;
  print_endline "\n";
  Gui.introduction_adv ();
  ANSITerminal.print_string [ANSITerminal.Blink; ANSITerminal.red] 
    (Parse.show_dice (Stats.get_player_roll big_state.dice));
  print_endline ("Other traders : ");
  ANSITerminal.print_string [ANSITerminal.red] ((Parse.show_dice 0));
  print_endline 
    ("\n \n Let us begin... make your market on the sum of all dice:  ");
  cli fermi big_state

(** [main] initiates the [play_game] function and prints a preamble introdution 
    describing the game. It pulls data from the "fermi.json" file before it 
    calls [play_game]. *)
let main () = 
  Gui.preamble ();
  play_game "fermi.json"
(* print_string  "> ";
   match read_line () with
   | exception End_of_file -> ()
   | file_name -> play_game file_name *)

(**  Initiates the game with random seed chosen for demonstration purposes. *)
let () = 
  Random.init 70;
  ANSITerminal.resize 150 240;
  main ()