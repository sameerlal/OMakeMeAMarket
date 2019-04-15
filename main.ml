open Yojson.Basic.Util
open Marketmaker
open Trader
open Command
open Parse

exception UnknownFile

type trader_players = {
  simple_ai : Trader.t;
}

type big_state = {
  mmstate : Marketmaker.t;
  traders : trader_players
}

let init_trader_players true_value = 
  { simple_ai = (Trader.init_trader true_value) }

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



let fsm fermi (state: big_state) = 
  let user_command = parse_user_input state in 
  if (Marketmaker.get_timestamp state.mmstate) = 69 then 
    (* TODO CHANGE STATE WITH NEW EVENTS *)
    state 
  else 
    match user_command with 
    | Command.Quit -> print_endline "QUIT"; exit 0
    | Command.Inventory -> (Marketmaker.display_data state.mmstate); state
    | Command.Set phr -> 
      let bid = int_of_string (List.nth phr 0) in 
      let ask = int_of_string (List.nth phr 1) in (* TODO  UPDATE STATE BELOW TO REFLECT NEW STATES *)
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
              simple_ai = new_trader_state
            }
          } in 
          match response with
          | "lift" -> print_endline "Trader has lifted your offer"; new_state
          | "hit" -> print_endline "Trader has hit your bid"; new_state
          | _ -> print_endline "error in trader's make trade"; exit 2
      end
    | _ -> print_endline "Done"; state


let rec cli fermi big_state = 
  ANSITerminal.(print_string [blue]
                  "\n\n ------------------- Statistics ------------------- \n");
  print_string ("Timestamp ");
  print_string ( string_of_int (Marketmaker.get_timestamp big_state.mmstate + 1));
  print_string (" |  Prev. bid/ask:  ");
  print_string (Marketmaker.stringify_bid_ask big_state.mmstate);
  print_string (" |  Outstanding Shares: " );
  print_string (string_of_int (Marketmaker.get_outstandingshares big_state.mmstate));
  match (Marketmaker.get_timestamp big_state.mmstate) with
  | 10 -> print_endline ("GAME OVER ");
    print_endline (" ADD STATISTICS AT END ");
    exit 0
  | _ -> cli fermi (fsm fermi big_state )



let init_big_state true_value = 
  {
    mmstate =  Marketmaker.init_market "init";
    traders = init_trader_players true_value;
  }


let play_game f =
  Parse.introduction ();
  let obtained_json = Yojson.Basic.from_file f in
  let fermi = Parse.from_json obtained_json in
  let true_value = Parse.get_answer fermi in 
  print_endline (Parse.get_question fermi);
  print_endline ("Let us begin... make your market:  ");
  cli fermi (init_big_state (int_of_string true_value))

let main () = 
  ANSITerminal.(print_string [blue]
                  "\n\nWelcome to the CamlCoin exchange! Here you will practice your maket
                  making skills \n");
  print_endline "Please enter the name of the file:  \n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

let () = main ()