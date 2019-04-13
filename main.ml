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

let init_trader_players = 
  { simple_ai = (Trader.init_trader ()) }

let rec parse_user_input state = 
  print_endline "\n \n Set market  \n";
  print_string "$ ";
  match read_line() with
  | exception End_of_file -> print_endline "Try again . . ."; parse_user_input state
  | user_response -> begin
      try Command.parse user_response with
      | Command.Empty -> parse_user_input state 	
      | Command.Malformed -> print_endline "Error in command."; parse_user_input state  
    end

let fsm fermi (state: big_state) = 
  print_endline ("reached fsm");
  let user_command = parse_user_input state in 
  if (Marketmaker.get_timestamp state.mmstate) = 69 then 
    (* TODO CHANGE STATE WITH NEW EVENTS *)
    state 
  else 
    match user_command with 
    | Command.Quit -> print_endline "QUIT"; exit 0
    | Command.Set phr -> 
      let bid = int_of_string (List.nth phr 0) in 
      let ask = int_of_string (List.nth phr 1) in (* TODO  UPDATE STATE BELOW TO REFLECT NEW STATES *)
      let trade_transaction = Trader.make_transaction (Marketmaker.get_timestamp state.mmstate) bid ask "blank" in
      let trader_response = Trader.make_trade state.traders.simple_ai trade_transaction in 
      begin
        match trader_response with 
        | None -> print_endline "No Trade occured"; state
        | Some (new_trader_state, response) -> 
          match response with
          (*  TODO   UPDATE THE STATES BELOW IWTH THE NEW MMSTATE AND TRADER STATE *)
          | "lift" -> print_endline "Trader has lifted your offer"; state
          | "hit" -> print_endline "Trader has hit your bid"; state 
          | _ -> print_endline "error in trader's make trade"; exit 2
      end
    | _ -> print_endline "Done"; state


let rec cli fermi big_state = 
  print_string ("Timestamp ");
  print_endline (" xxxx");
  print_string (" Prev. bid/ask:  ");
  print_endline (" xxxx ");
  print_string (" Outstanding Shares: " );
  print_endline (" xxxx ");
  match (Marketmaker.get_timestamp big_state.mmstate) with
  | 10 -> print_endline ("GAME OVER ");
    print_endline (" ADD STATISTICS AT END ");
    exit 0
  | _ -> cli fermi (fsm fermi big_state )



let init_big_state = 
  {
    mmstate =  Marketmaker.init_market "init";
    traders = init_trader_players;
  }


let play_game f =
  Parse.introduction ();
  let obtained_json = Yojson.Basic.from_file f in
  let fermi = Parse.from_json obtained_json in
  print_endline (Parse.get_question fermi);
  print_endline ("Let us begin... make your market:  ");
  cli fermi init_big_state

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