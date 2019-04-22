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

let opponents = 3

exception UnknownFile

type trader_players = Trader.trader_players

type big_state = {
  mmstate : Marketmaker.t;
  traders : trader_players;
  dice : Stats.dice_data
}

(**[init_trader_players roll_list] is the group of traders playing with the 
   player. *)
let init_trader_players (rolls:int list) : trader_players = 
  { simple_ai = (Trader.init_trader (List.nth rolls 0));
    ai1 = (Trader.init_trader (List.nth rolls 1));
    ai2 = (Trader.init_trader (List.nth rolls 2));
  }  


(**[parse_user_input state] is the parsing of the user's input commands. *)
let rec parse_user_input state = 
  print_endline "\n \n Make a market:   \n";
  print_string "> ";
  match read_line() with
  | exception End_of_file -> print_endline "Try again . . ."; parse_user_input state
  | user_response -> begin
      try Command.parse user_response with
      | Command.Empty -> parse_user_input state 	
      | Command.Malformed -> print_endline "Error in command."; parse_user_input state  
    end


(**[fsm fermi state] is the looping fsm that keeps the game playing based on the 
   commands put in by the user. *)
let fsm fermi (state: big_state) = 
  let user_command = parse_user_input state in 
  if (Marketmaker.get_timestamp state.mmstate) = 69 then 
    (* TODO IN NEXT PHASE:  CHANGE STATE WITH NEW EVENTS *)
    state 
  else 
    match user_command with 
    | Command.Quit -> print_endline "QUIT"; exit 0
    | Command.Inventory -> (Marketmaker.display_data state.mmstate); state
    | Command.History -> (Marketmaker.stringify_bidask_history state.mmstate); state
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
      let trade_transaction = Trader.make_transaction (Marketmaker.get_timestamp state.mmstate) bid ask "blank" in
      let trader_response = Trader.make_trade_dumb state.traders.simple_ai trade_transaction in 
      begin
        match trader_response with 
        | None -> print_endline "No Trade occured"; 
          {
            state with 
            mmstate = (Marketmaker.increment_timestep state.mmstate)
          }
        | Some (new_trader_state, response) -> 
          let new_MM_state = Marketmaker.transaction 
              (Marketmaker.generate_receive_transaction (Marketmaker.get_timestamp state.mmstate) 
                 response bid ask) state.mmstate in 
          let new_state = {
            mmstate = new_MM_state;
            traders = {
              state.traders with
              simple_ai = new_trader_state

            };
            dice = state.dice
          } in 
          match response with
          | "lift" -> print_endline "Trader bought a CamlCoin (lifted your offer)"; new_state
          | "hit" -> print_endline "Trader sold a CamlCoin (hit your bid)"; new_state
          | _ -> print_endline "error in trader's make trade"; exit 2
      end

let cycle_traders (bstate:big_state) : unit = 
  let tr = bstate.traders in
  print_endline (string_of_int tr.simple_ai.hidden_number);
  print_endline (string_of_int tr.ai1.hidden_number);
  print_endline (string_of_int tr.ai2.hidden_number)

(**[cli fermi big_state] results in the printing of the statistics for a player at every command input and initializes the game. *)
let rec cli fermi big_state = 
  ANSITerminal.(print_string [blue]
                  "------------------- Statistics ------------------- \n");
  cycle_traders big_state;
  print_string ("\nTimestamp ");
  print_string ( string_of_int (Marketmaker.get_timestamp big_state.mmstate + 1));
  print_string (" |  Prev. bid/ask:  ");
  print_string (Marketmaker.stringify_bid_ask big_state.mmstate);
  print_string (" |  CamlCoins Accumulated: " );
  print_string (string_of_int (Marketmaker.get_outstandingshares big_state.mmstate));
  match (Marketmaker.get_timestamp big_state.mmstate) with
  | 10 -> print_endline ("GAME OVER ");
    (Marketmaker.display_data big_state.mmstate);
    exit 0
  | _ ->  cli fermi (fsm fermi big_state )


(**[init_big_state true_value] is an initial big_state with the true value of the security. *)
let init_big_state true_value = 
  let dice_state = Stats.mega_roll opponents in
  {
    mmstate =  Marketmaker.init_market "init";
    traders = init_trader_players dice_state.other_rolls;
    dice = dice_state;
  }


let play_game f =
  let obtained_json = Yojson.Basic.from_file f in
  let fermi = Parse.from_json obtained_json in
  let big_state = init_big_state opponents in 
  ANSITerminal.erase Above;
  Parse.get_intro fermi;
  Unix.sleep 1;
  print_endline "\n";
  Gui.introduction_adv ();
  ANSITerminal.print_string [ANSITerminal.green] (string_of_int (Stats.get_player_roll big_state.dice));
  print_endline ("\n \n Let us begin... make your market:  ");
  cli fermi big_state

let main () = 
  Gui.preamble ();
  play_game "fermi.json"
(* print_string  "> ";
   match read_line () with
   | exception End_of_file -> ()
   | file_name -> play_game file_name *)

let () = main ()