(******************************************************************
                            Marketmaker
    Representation of the marketmaker - player - during the course of the game.

    This module keeps track of the marketmaker's bid/ask prices and the 
    transactions that take place between the player and traders. It contains 
    structs to hold this data and has functions to implement changes to and 
    display the data.
 ********************************************************************)
(** The abstract type of values representing adventures. *)
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

(* Trade variant.  To be sent to engine *)
type send_market = {
  timestamp : int;
  transaction: bidask;
}

(* The transaction type. Holds information about a transaction between 
   a trader and marketmaker. *)
type receive_transaction =  {
  timestamp : int;
  trade_type : string; (* hit the bid or lifted offer *)
  transaction : bidask;
}

(**[init_market game] is the initial state of the market after a [game] is 
   started. It initializes all fields to its starting values and marks
   the "trade_type" field in the record as "init".  *)
val init_market : string -> t

(**[get_timestamp market] is the value of the timestamp in the market  *)
val get_timestamp : t -> int

(**[transaction market trade] is the new state of the marketmaker after a trade.
   It takes in a type t [market] and a [trade] and sets a new inventory, 
   bidask, timestamp, orderbook and curr_profit for the marketmaker.
   We update the timestamp here.
*)
val transaction : receive_transaction -> t -> t

(**[generate_receive_transaction timestamp trade_type bid ask] returns a 
   transaction variant with the given arguments. *)
val generate_receive_transaction : int -> string -> int -> int -> 
  receive_transaction

(**[stringify_bid_ask market] is a string of the current bid and ask. *)
val stringify_bid_ask : t -> string

(**  [get_outstandingshares market] returns the outstanding shares of the 
     market maker's state [market]. *)
val get_outstandingshares : t -> int

(**[increment_timestep market] is a type t [market] with the timestamp 
   incremented. *)
val increment_timestep : t -> t

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
val display_data : t -> unit

(**  [stringify_bidask_history market] returns a history of past market markets
     as seen in [market] state.  *)
val stringify_bidask_history : t -> unit

(** [exchange_mm_excess market sum_dice] will return a new market.t object 
    with excess/deficient shares of camlcoin swapped for cash in accordance 
    with the true market value, [sum_dice].  The final excess coins will be 0 
    (invariant).  *)
val exchange_mm_excess : t -> int -> t