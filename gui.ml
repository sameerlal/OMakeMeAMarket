open Pervasives


(* Make market around this value *)
let tutorial_value = 100

(*  TUTORIAL ALGORITHM :   [this is actually not trivial]  *)

type trade_feedback = Low | High | BigSpread | Okay

(* If interval is bad, then None.  If interval is good but spread too large then
   Some false .  Otherwise Some true. *)
let check_good_interval (bid:int) (ask:int) =
  if (bid <= tutorial_value ) 
  && (ask >= tutorial_value ) 
  && ((ask - bid) < 5) then
    Some true else
  if (bid <= tutorial_value ) 
  && (ask >= tutorial_value ) 
  && ((ask - bid) >= 5)then Some false 
  else None

(* Take in bid and ask and return string for feedback. *)
let eval_trade bid ask =
  match (check_good_interval bid ask) with
  | Some x -> 
    if x = true then "Your market is perfect!  "
    else "Your market is good, but your spread is too high.  
            Traders might not want to trade with you.  "
  | None -> 
    if bid > tutorial_value then 
      "A lot of traders are hitting your bid (selling to you).  
              This means they're selling for a high price!  Adjust your market downwards.  "
    else 
      "A lot of traders are lifting your offer (buying from you).
            This means they're buying for a low price, a bargain!  Adjust your market upwards.  "

let parse_input_tut str = 
  let wordlst = String.split_on_char ' ' (String.trim str) in 
  let filtered = List.filter (fun s -> String.length s >= 1) wordlst in 
  if List.length filtered > 0 then 
    let cmd = List.hd filtered in 
    match cmd with
    | "set" -> if List.length filtered > 0 then 
        let obj = begin match filtered with 
          | [] -> []
          | h::t -> t 
        end
        in Some (obj) else None 
    | "exit" -> Some ["exit"]
    | _ -> None
  else None

(* Take user input string.  Return true to continue game or false to quit *)
let query str =
  match (parse_input_tut str) with
  | None -> print_endline "Invalid input, try again"; true
  | Some text -> begin 
      if (List.length text = 1) then (print_endline "Quitting tutorial..."; false)
      else 
        let response = eval_trade (int_of_string (List.nth text 0)) (int_of_string (List.nth text 1)) in
        print_endline response; true
    end

let rec tutorial_fsm unit = 
  print_endline ("Enter a bid/offer ");
  match query (read_line ()) with
  | true -> tutorial_fsm ();
  | false -> ()


let tutorial_preamble unit = 
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.magenta] "~ TUTORIAL INTRODUCTION ~\n";
  print_endline (" The goal of this game is to make a market for traders who want to buy and sell
  CamlCoin.  For now, suppose we tell you a priori that the true value of CamlCoin is $10.  ");
  print_endline ("Your goal is to continuously provide bid/ask spreads and hopefully make a profit on
   your spread.  ");
  print_endline ("We use the notation 'set x y' to mean you, the marketmaker, will BUY at x and SELL at y");
  print_endline ("--> this means traders will BUY at y and SELL at x");
  print_endline ("Suppose your market is 'set 20 40'.  In this case, the sell price of $20 is
    greater than the intrinsic value.  Sellers get a great deal!  They can sell something worth $10 for
    $20.  Expect a lot of sells.  Adjust your interval downwards. ");
  print_endline ("Suppose your market is 'set 5 8'.  In this case, the buy price $8 is much less than
    the intrinsic value of $10.  Buys are getting a bargain!  Expect a lot of buys.  Adjust your interval upwards.");
  print_endline ("Suppose your market is 'set 9 11'.  Your market is good!  With some variance,
  buyers will buy at $11 which is very close to the actual price.  Sellers will sell at $9.  Expect roughly even
  buyers and sellers.  You will make $11 - $9 = $2 per transaction.  ");
  print_endline ("Starting tutorial .. .  ");
  print_endline ("Now you try.  We won't tell you the actual price.  ");
  tutorial_fsm ();
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.magenta] "End of tutorial. "

(* Text to display  *)

let preamble unit = 
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.white] "Welcome!";
  ANSITerminal.(print_string [black; white]
                  "\n\n Welcome to the CamlCoin exchange! Here you will practice your maket making skills \n"); ()

let introduction unit = 
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.white] ("Today, you are the sole market maker of a coin called CamelCoin. ");
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.white] ("You will be making a market for several traders in USD.  ");
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.white] ("You are competing with other high frequency trading firms such as Citadel, Jane Street and Optiver, so be fast! ");
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.white] ("Type in 'help' for a list of commands or 'tutorial' to begin the tutorial" );
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.white] ("We will assume familiarity, so let us begin!  ");
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.white] ("The true cost of a CamelCoin is equal to the answer of the following question:  "); ()

let display_help unit =
  ANSITerminal.print_string [ANSITerminal.red; ANSITerminal.blue] ("The list of possible help items are: ");
  ANSITerminal.print_string [ANSITerminal.red; ANSITerminal.yellow] ("
      Inventory  \n
      Orderbook \n
      Profit \n
      Get (bid) (ask) \n
      Help \n
      Statistics \n
      Tutorial \n
  ");
  ()