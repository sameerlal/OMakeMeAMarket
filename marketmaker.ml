open Pervasives

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
  timestamp: int;
  curr_profit: int;
  orderbook: orderbook
}


(* Trade variant.  To be sent to engine *)

type send_market = {
  timestamp : int;
  transaction: bidask;
}

type receive_transaction =  {
  timestamp : int;
  trade_type : string; (* hit the bid or lifted offer *)
  transaction : bidask;
}


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
   started. *)
let init_market game : t =
  {
    currbidask = {
      trade_type = "init";
      bid = 0;
      ask = 0;
      spread = 0;
    };
    timestamp = 0;
    curr_profit = 0;
    orderbook = {
      outstanding_shares = 0
    }
  }

type result = Legal of t | Illegal



let calculate_new_profit (transaction:receive_transaction) market : t =
  failwith "Unimplemented"

(**[readjust_spread transaction market] is a market with the updated spread for the 
   marketmaker after a bidask change. It has the new bid-offer pair after the 
   marketmaker has finished a trade for the security at a certain price. The 
   goal of readjustment is to prevent hacks and hopefully be above scratch. It 
   takes in a new [bid] and [ask] price set by the player and the current 
   [market] of type t.*)
let readjust_spread (transaction:receive_transaction) (market:t) : t =
  failwith "Unimplemented"



(**[transaction market trade] is the new state of the marketmaker after a trade.
   It takes in a type t [market] and a [trade] and sets a new inventory, 
   bidask, timestamp, orderbook and curr_profit for the marketmaker.
   We update the timestamp here.
*)
let transaction (transaction:receive_transaction) (market:t) =
  {
    currbidask = {
      trade_type = transaction.trade_type;
      bid = transaction.transaction.bid;
      ask = transaction.transaction.ask;
      spread = transaction.transaction.spread;
    };
    timestamp = transaction.timestamp + 1;
    curr_profit = market.curr_profit; (* TODO *)
    orderbook = {
      outstanding_shares = market.orderbook.outstanding_shares 
                           + (if transaction.trade_type = "hit" then 1 else -1 ); (* TODO *)
    }
  }

let increment_timestep (market : t) =
  {market with 
   timestamp = market.timestamp + 1
  }

(**[send_market market] is the new marketmaker after the player has set new 
   bid/ask prices. *)
let send_market market =
  failwith "Unimplemented"

let get_profit market =
  market.curr_profit

let get_outstandingshares market =
  market.orderbook.outstanding_shares

let get_orderbook (market : t) =
  market.orderbook

let stringify_bid_ask (market : t) =
  (string_of_int market.currbidask.bid) ^ "@" ^ (string_of_int market.currbidask.ask)

let get_timestamp (market : t) = 
  market.timestamp






