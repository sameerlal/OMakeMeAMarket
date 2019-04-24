open Pervasives


(* Make market around this value *)
let tutorial_value = 100

(*  TUTORIAL ALGORITHM TO ENSURE PLAYER MAKES PROGRESS TOWARDS THE PROPER 
    INTERVAL  *)

type trade_feedback = Low | High | BigSpread | Okay

(* [check_good_interval bid ask] is an option of either None or Some bool 
    option. If interval is bad, then None.  If interval is good but spread 
    too large then Some false .  Otherwise Some true. *)
let check_good_interval (bid:int) (ask:int) =
  if (bid <= tutorial_value ) 
  && (ask >= tutorial_value ) 
  && ((ask - bid) < 15) then
    Some true else
  if (bid <= tutorial_value ) 
  && (ask >= tutorial_value ) 
  && ((ask - bid) >= 15)then Some false 
  else None

(* [eval_trade bid ask] is a string message that acts as feedback to a 
    [bid] and [ask]. *)
let eval_trade bid ask =
  match (check_good_interval bid ask) with
  | Some x -> 
    if x = true then "Perfect! Your market is good and has tight spread!  Let's start the real game!  "
    else "\nYour market is good, but your spread is too high.  
            Traders might not want to trade with you.  \n"
  | None -> 
    if bid > tutorial_value then 
      "\nA lot of traders are hitting your bid (selling to you).  
              This means they're selling for a high price!  Adjust your market downwards.  \n"
    else 
      "\nA lot of traders are lifting your offer (buying from you).
            This means they're buying for a low price, a bargain!  Adjust your market upwards.  \n"

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
  | Some text -> 
    if List.length text = 1 then 
      false
    else 
      begin 
        if (List.length text = 1) then (print_endline "Quitting tutorial..."; false)
        else 
          let response = eval_trade (int_of_string (List.nth text 0)) (int_of_string (List.nth text 1)) in
          match (String.get response 0) with 
          | 'P' -> (print_endline response); false
          | _ -> (print_endline response); true
      end

let rec tutorial_fsm unit = 
  print_endline ("Enter a bid/offer ");
  match query (read_line ()) with
  | true -> tutorial_fsm ();
  | false -> ()


let tutorial_preamble unit = 
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ANSITerminal.magenta] 
    "~ TUTORIAL INTRODUCTION ~\n";
  ANSITerminal.print_string [ANSITerminal.yellow] 
    ("The goal of this game is to make a market for traders who want to buy and sell " ^ "CamlCoin. \nFor now, suppose we tell you that the true value of CamlCoin is $10.\n");
  ANSITerminal.print_string [ANSITerminal.yellow] 
    ("Your goal is to continuously provide bid/ask spreads and hopefully make a profit on " ^ "your spread.\n");
  ANSITerminal.print_string [ANSITerminal.yellow] 
    ("We use the notation 'set x y' to mean you, the marketmaker, will BUY at x and SELL at y \n \n");
  ANSITerminal.print_string [ANSITerminal.yellow] 
    ("This means traders will BUY at y and SELL at x! \n \n");
  ANSITerminal.print_string [ANSITerminal.yellow] 
    ("Suppose your market is 'set 20 40'.  \n \nIn this case, the sell price of $20 is greater than the intrinsic value.  \nSellers get a great deal! They can sell something worth $10 for"
      ^ "$20.  Expect a lot of sells.  Adjust your interval downwards.\n");
  ANSITerminal.print_string [ANSITerminal.yellow] 
    ("\nSuppose your market is 'set 5 8'.  \n \nIn this case, the buy price $8 is much less than " ^ "the intrinsic value of $10.  Buys are getting a bargain! Expect a lot of buys. Adjust your interval upwards.");
  ANSITerminal.print_string [ANSITerminal.yellow] 
    ("\n\nSuppose your market is 'set 9 11'.  \n\nYour market is good!  With some variance, " ^ "buyers will buy at $11 which is very close to the actual price. Sellers will sell at $9. Expect roughly even " ^ " buyers and sellers.  \nYou will make $11 - $9 = $2 per transaction.");
  ANSITerminal.print_string [ANSITerminal.yellow] 
    ("\n\n>>>> Starting tutorial <<<<  ");
  ANSITerminal.print_string [ANSITerminal.yellow] 
  ("\nNow you try.  We won't tell you the actual price... \n");
  tutorial_fsm ();
  ()

(* Text to display  *)

let preamble unit = 
  ANSITerminal.erase Above;
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.yellow] 
    "Welcome! \n";
  ANSITerminal.(print_string [black; yellow]
    "Welcome to the CamlCoin exchange! Here you will practice your maket making skills \n"); ()
let display_help unit =
  ANSITerminal.print_string [ANSITerminal.red; ANSITerminal.blue] 
    ("The list of possible commands are: ");
  ANSITerminal.print_string [ANSITerminal.red; ANSITerminal.yellow] ("
      set [bid] [ask] \n
      inventory  \n
      history \n
      help \n
      cheat \n
      tutorial\n");
  ()

let introduction unit = 
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.green] 
    ("Today, you are the sole market maker of a coin called CamelCoin. \n");
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.green] 
    ("You will be making a market for several traders in USD.  \n");
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.green] 
    ("You are competing with other high frequency trading firms such as Citadel, Jane Street and Optiver, so be fast! \n");
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.green] 
    ("Type in 'help' for a list of commands or 'tutorial' to begin the tutorial \n" );
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.green] 
    ("We will assume familiarity, so let us begin!  \n");
  display_help ();
  ANSITerminal.print_string [ANSITerminal.red] 
    ("The true cost of a CamelCoin is equal to the answer of the following question:  \n \n");
  ()

let introduction_adv unit = 
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.green] 
    ("Today, you are the sole market maker of a coin called CamelCoin. \n");
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.green] 
    ("You will be making a market for several traders in USD.  \n");
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.green] 
    ("You are competing with other high frequency trading firms, so be fast! \n");
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.green] 
    ("Type in 'help' for a list of commands or 'tutorial' to begin the tutorial \n" );
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.green] 
    ("We will assume familiarity, so let us begin! \n");
  display_help ();
  ANSITerminal.print_string [ANSITerminal.red] ("Simulating dice rolls....\n");
  ANSITerminal.print_string [ANSITerminal.red] 
    ("Each trader has rolled a dice.\n");
  ANSITerminal.print_string [ANSITerminal.red] ("You rolled a : \n \n");
  ()
