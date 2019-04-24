open Pervasives
open Sys

(* holds bid ask struct *)
type bidask = {
  trade_type : string;
  bid: int;
  ask: int;
  spread: int;
}

(* keeps track of market maker's holdings *)
type orderbook = {
  outstanding_shares: int;
}

(* Holds market maker type *)
type t = {
  currbidask: bidask;
  bid_ask_history: bidask list;
  timestamp: int;
  curr_profit: int;
  orderbook: orderbook
}

type send_market = {
  timestamp : int;
  transaction: bidask;
}

type receive_transaction =  {
  timestamp : int;
  trade_type : string; (* hit the bid or lifted offer *)
  transaction : bidask;
}

(**[generate_receive_transaction timestamp trade_type bid ask] returns a 
   transaction variant with the given arguments. *)
let generate_receive_transaction timestamp trade_type bid ask =
  {
    timestamp = timestamp;
    trade_type = trade_type;
    transaction = {
      trade_type = trade_type;
      bid = bid;
      ask = ask;
      spread = ask - bid;
    }
  }

(**[init_market game] is the initial state of the market after a [game] is 
   started. It initializes all fields to its starting values and marks
   the "trade_type" field in the record as "init".  *)
let init_market game : t =
  {
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
  }

type result = Legal of t | Illegal

(**[display_data state] is a unit with side-effects of displaying data about 
   transactions and the currents status of the player and market. 

   This uses ANSITerminal to pretty-print the market maker's statistics.
   It currently includes the following data:
   - Current Bid/Ask
   - Spread
   - Timestamp
   - Current Profit
   - # Coins accumulated
*)
let display_data (state : t) = 
  ANSITerminal.(print_string 
                  [red]
                  "\n\n ------------------- Market Maker 
                  Statistics ------------------- \n");
  print_string ("Current Bid/Ask : ");
  print_string (" | Bid: " ^ (string_of_int state.currbidask.bid));
  print_string (" | Ask: " ^ (string_of_int state.currbidask.ask));
  print_endline ("| Spread:  " ^ (string_of_int state.currbidask.spread));
  print_string (" Time Stamp:   " ^ (string_of_int state.timestamp));
  print_endline (" | Current Profit:  " ^ (string_of_int state.curr_profit));
  print_endline ("# Coins Accumulated: " ^ 
                 (string_of_int state.orderbook.outstanding_shares));
  ANSITerminal.(print_string 
                  [green] 
                  "\n \n --------------------------
                  -------------------------------\n " )


(**[readjust_spread transaction market] is a market with the updated spread for 
   the marketmaker after a bidask change. It has the new bid-offer pair after 
   the marketmaker has finished a trade for the security at a certain price. The 
   goal of readjustment is to prevent hacks and hopefully be above scratch. It 
   takes in a new [bid] and [ask] price set by the player and the current 
   [market] of type t.

   For now, we are not implementing this as we have included this functionality 
   in a different function for cleaner code.
*)
let readjust_spread (transaction:receive_transaction) (market:t) : t =
  failwith "Unimplemented"

(**[transaction market trade] is the new state of the marketmaker after a trade.
   It takes in a type t [market] and a [trade] and sets a new inventory, 
   bidask, timestamp, orderbook and curr_profit for the marketmaker.
   We update the timestamp here.
*)
let transaction (transaction:receive_transaction) (market:t) =
  let new_curr_bid_ask = 
    {
      trade_type = transaction.trade_type;
      bid = transaction.transaction.bid;
      ask = transaction.transaction.ask;
      spread = transaction.transaction.spread;
    } in 
  {
    currbidask = new_curr_bid_ask;
    bid_ask_history = market.bid_ask_history @ [new_curr_bid_ask];
    timestamp = transaction.timestamp + 1;
    curr_profit = market.curr_profit + (if transaction.trade_type = "lift" 
                                        then transaction.transaction.ask 
                                        else -1*transaction.transaction.bid); 
    (* TODO *)
    orderbook = {
      outstanding_shares = market.orderbook.outstanding_shares 
                           + 
                           (if transaction.trade_type = "hit" then 1 else -1 );
    }
  }

(** [exchange_mm_excess market sum_dice] will return a new market.t object 
    with excess/deficient shares of camlcoin swapped for cash in accordance 
    with the true market value, [sum_dice].  The final excess coins will be 0 
    (invariant).  *)
let exchange_mm_excess (market : t) (sum_dice : int) = 
  let excess = market.orderbook.outstanding_shares in 
  {
    market with 
    curr_profit = market.curr_profit + (excess*sum_dice);
    orderbook = {
      outstanding_shares = 0;
    }
  }

(**[increment_timestep market] is a type t [market] with the timestamp 
   incremented. *)
let increment_timestep (market : t) =
  {market with 
   timestamp = market.timestamp + 1
  }

(**[send_market market] is the new marketmaker after the player has set new 
   bid/ask prices. *)
let send_market market =
  failwith "Unimplemented"

(**  [get_profit market] returns the profit of the market maker's state 
     [market]. *)
let get_profit market =
  market.curr_profit

(**  [get_outstandingshares market] returns the outstanding shares of the 
     market maker's state [market]. *)
let get_outstandingshares market =
  market.orderbook.outstanding_shares

(**  [get_orderbook market] returns the orderbook variant of the market
     maker's state [market]. *)
let get_orderbook (market : t) =
  market.orderbook

(**  [stringify_bidask_history market] returns a history of past market markets
     as seen in [market] state.  *)
let stringify_bidask_history (market : t) =
  ANSITerminal.print_string [ANSITerminal.blue; ANSITerminal.cyan] 
    " ----------------- HISTORY ------------------ \n";
  let rec ba_helper (ls : bidask list) = 
    match ls with
    | [] -> ()
    | h::t -> 
      ANSITerminal.print_string [ANSITerminal.blue; ANSITerminal.yellow] 
        (h.trade_type ^ " " ^ (string_of_int h.bid) ^ "@" 
         ^ (string_of_int h.ask) ^ " with spread " ^
         (string_of_int h.spread) ^ "\n"); 
      ba_helper t

  in ba_helper (market.bid_ask_history)

(**[stringify_bid_ask market] is a string of the current bid and ask. *)
let stringify_bid_ask (market : t) =
  (string_of_int market.currbidask.bid) ^ "@" ^ 
  (string_of_int market.currbidask.ask)

let get_timestamp (market : t) = 
  market.timestamp

