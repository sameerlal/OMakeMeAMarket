open Pervasives


(* Make market around this value *)
let tutorial_value = 100

(*  TUTORIAL ALGORITHM :   [this is actually not trivial]  *)


(* If interval is bad, then None.  If interval is good but spread too large then
   Some false .  Otherwise Some true. *)
let check_good_interval bid ask =
  if (bid <= tutorial_value ) 
  && (ask >= tutorial_value ) 
  && ( (ask - bid) < 5 ) then
    Some true else
  if (ask - bid >= 5 ) then Some false 
  else None





let tutorial_preamble unit = 
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.magenta] "~ TUTORIAL INTRODUCTION ~";
  print_endline ("TODO ");
  print_endline ("Enter a starting bid/offer ");
  ()





(* Text to display  *)

let preamble unit = 
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.white] "Welcome!";
  ANSITerminal.(print_string [black; white]
                  "\n\nWelcome to the CamlCoin exchange! Here you will practice your maket making skills \n");
  print_endline "Please enter the name of the file:  \n"; ()


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