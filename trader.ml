(********************************************************************
   Trader 

   The Trader module contains the trader type, getters and setters
   as well as basic statistics analysis.  It also includes at least 
   four AI methods which are used as the backend for trading opponents.
   There is also a contention function which decides which trader
   gets to trade if multiple traders wish to transact.
 ********************************************************************)

open Pervasives
open Random

let num_opponents = 3

type bidask = {
  bid: int;
  ask: int;
  spread: int;
}

type transaction = {
  timestamp : int;
  bidask: bidask;
  order_type : string; (* bid or ask *)
}

type orderbook = {
  transactions : transaction list;
  buys: int;
  sells: int
}

type t = {
  id : string;
  hidden_number : int;
  avg_buy_value : int;
  profit : int;
  cash : int;
  inventory : int; (* Total number of shares owned *)
  orderbook : orderbook;
}

type trader_players = {
  simple_ai : t;
  ai1 : t;
  ai2 : t;
}


(**[init_trader hidden identifier] initalizes a trader record with hidden number 
   [hidden] and id equal to [identifier] *)
let init_trader hidden identifier =
  {id = identifier; hidden_number = hidden; avg_buy_value = 0; profit = 0; 
   cash = 1000000; inventory = 0; orderbook = 
                                    {transactions = []; buys = 0; sells = 0}}


(**[get_curr_profit cash init_cash msell_price inventory] is the current 
   running profit of the trader. *)
let get_curr_profit cash ?(init_cash = 1000000) msell_price inventory =
  inventory * msell_price + cash - init_cash

(**[make_bidask bid ask] returns a type bidask with respective record values 
   equal to [bid] and [ask]. *)
let make_bidask bid ask =
  {
    bid = bid;
    ask = ask;
    spread = ask - bid;
  }
(**[make_transaction timestamp bid ask order_type] returns a variant transaction
   that takes in a [timestamp], [order_type] and a [bid] and [ask]. *)
let make_transaction timestamp bid ask order_type =
  {
    timestamp = timestamp;
    bidask = (make_bidask bid ask);
    order_type = order_type;
  }

(**[get_bids lst trans_lst] is the list of bids from [trans_lst]. *)
let get_bids trans_lst =
  (* match trans_lst with
     | [] -> list
     | h::t -> h.bidask.bid::list *)
  List.map (fun x -> x.bidask.bid ) trans_lst

(**[make_trade trader transaction] is an option either of Some new type t trader 
   (or the old trader depending on whether the trader will accept the 
   marketmaker's bid for the security) or None which indicates whether trade 
   was accepted.

   AI Description:
   This is one AI choice where the trader will trade according to the trader's 
   current positions.  We describe the trader to be "flaky" as the trader does
   not want to hold excess shares, but still wants to participate in trades. 
   The trader will sell shares after accumulating a certain amount and will
   buy shares if deficient.   In addition, the trader will purchase shares if
   the current offer is less than the average holding.

   This AI is meant to mimic a trader with small starting capital.
*)
let make_trade trader transaction =
  let sell_value = transaction.bidask.bid in
  let buy_value = transaction.bidask.ask in
  let inv = trader.inventory in 
  let book = trader.orderbook.transactions in
  let avg_val = trader.avg_buy_value in
  if trader.orderbook.buys = 0 then 
    if buy_value < trader.hidden_number then
      let new_buys = trader.orderbook.buys + 1 in
      let new_sells = trader.orderbook.sells in 
      let new_cash = trader.cash - buy_value in
      let newtransaction = {
        transaction with order_type = "bid"
      } in
      let t = {trader with avg_buy_value = 
                             (avg_val* (new_buys - 1) + buy_value) / new_buys; 
                           profit = (get_curr_profit new_cash sell_value inv+1); 
                           inventory = inv - 1; 
                           orderbook = {transactions = newtransaction::book; 
                                        buys = new_buys; sells = new_sells}} in
      Some (t, "hit")
    else None
  else if sell_value > avg_val && inv > 3 then
    let new_buys = trader.orderbook.buys in
    let new_sells = trader.orderbook.sells + 1 in 
    let new_cash = trader.cash + sell_value in
    let newtransaction = {
      transaction with order_type = "ask"
    } in
    let t = {trader with profit = (get_curr_profit new_cash sell_value inv-1); 
                         inventory = inv + 1; 
                         orderbook = {transactions = newtransaction::book; 
                                      buys = new_buys; sells = new_sells}} in 
    Some (t, "lift")
  else if buy_value < avg_val && trader.cash > buy_value then 
    let new_buys = trader.orderbook.buys + 1 in
    let new_sells = trader.orderbook.sells in 
    let new_cash = trader.cash - buy_value in
    let newtransaction = {
      transaction with order_type = "bid"
    } in
    let t = {trader with avg_buy_value = (avg_val + buy_value) / new_buys; 
                         profit = (get_curr_profit new_cash sell_value inv+1); 
                         inventory = inv - 1; 
                         orderbook = {transactions = newtransaction::book; 
                                      buys = new_buys; sells = new_sells}} in
    Some (t, "hit")
  else None


(** [make_trade_dumb trader transaction] is an option either of Some new type 
    t trader (or the old trader depending on whether the trader will accept the 
    marketmaker's bid for the security) or None which indicates whether trade 
    was accepted.

    AI Description:
    [make_trader_dumb] is described as "dumb" because the trader does not keep
    track of its holdings.  It looks at its hidden number and calculates the
    expected value of the sum of all remaining dice. It then transacts according
    to this calculated expected value, without keeping track of its holdings.

    This is meant to simulate a perfect trader who only trades according to EV.
*)
let make_trade_dumb (trader:t) (transaction:transaction) = 
  if float_of_int transaction.bidask.bid > 
     (float_of_int num_opponents)*.3.5 +. (float_of_int trader.hidden_number) 
  then 
    Some(trader, "hit")
  else if float_of_int transaction.bidask.ask < 
          (float_of_int num_opponents)*.3.5 +. 
          (float_of_int trader.hidden_number) 
  then
    Some (trader, "lift")
  else
    None




(**[make_trade_optimist t transaction] is an option of None or Some pair of 
   dummy type t [trader] and a string denoting whether the trader will lift or 
   hit. 

   AI Description:
   [make_trade_optimist] calculates its EV by assuming that all other players
   have the same dice as what the trader rolled.  It will only trade on 
   tight spreads, because it assumes that the market maker is perfect.

   This is meant to simulate an optimist trader who "goes with the hype".
*)
let make_trade_optimist (trader:t) (transaction:transaction) = 
  let time = Random.int 2 in 
  let seed = (time) mod 2 in 
  if (abs (transaction.bidask.ask - trader.hidden_number*4) < 10) &&
     (abs (transaction.bidask.bid - trader.hidden_number*4) < 10 ) then
    match seed with
    | 0 -> Some (trader, "hit")
    | 1 -> Some (trader, "lift")
  else 
  if transaction.bidask.ask < trader.hidden_number + 10 
  then Some (trader, "lift")
  else if transaction.bidask.bid > trader.hidden_number - 10
  then Some (trader, "hit")
  else None


(**  [make_trade_weary trader transaction] is an option of None or Some pair of 
     dummy type t [trader] and a string denoting whether the trader will lift or 
     hit.

     AI Description:
     [make_trade_weary] trades cautiously.  It keeps tracks of previous trades
     and factors in its current inventory, only playing in low volatility 
     environments. 

     This is meant to simulate a cautious, risk adverse trader.
*)
let make_trade_weary trader transaction =
  let sell_value = transaction.bidask.bid in
  let buy_value = transaction.bidask.ask in
  let inv = trader.inventory in 
  let book = trader.orderbook.transactions in
  let avg_val = trader.avg_buy_value in
  if trader.orderbook.buys = 0 then 
    if buy_value < trader.hidden_number then
      let new_buys = trader.orderbook.buys + 1 in
      let new_sells = trader.orderbook.sells in 
      let new_cash = trader.cash - buy_value in
      let newtransaction = {
        transaction with order_type = "bid"
      } in
      let t = {trader with avg_buy_value = 
                             (avg_val* (new_buys - 1) + buy_value) / new_buys; 
                           profit = (get_curr_profit new_cash sell_value inv+1); 
                           inventory = inv - 1; 
                           orderbook = {transactions = newtransaction::book; 
                                        buys = new_buys; sells = new_sells}} in
      Some (t, "hit")
    else None
  else if sell_value > avg_val && inv > 0 then
    let new_buys = trader.orderbook.buys in
    let new_sells = trader.orderbook.sells + 1 in 
    let new_cash = trader.cash + sell_value in
    let newtransaction = {
      transaction with order_type = "ask"
    } in
    let t = {trader with profit = (get_curr_profit new_cash sell_value inv-1); 
                         inventory = inv + 1; 
                         orderbook = {transactions = newtransaction::book; 
                                      buys = new_buys; sells = new_sells}} in 
    Some (t, "lift")
  else if buy_value < avg_val && trader.cash > (buy_value * 2) then 
    let new_buys = trader.orderbook.buys + 1 in
    let new_sells = trader.orderbook.sells in 
    let new_cash = trader.cash - buy_value in
    let newtransaction = {
      transaction with order_type = "bid"
    } in
    let t = {trader with avg_buy_value = (avg_val + buy_value) / new_buys; 
                         profit = (get_curr_profit new_cash sell_value inv+1); 
                         inventory = inv - 1; 
                         orderbook = {transactions = newtransaction::book; 
                                      buys = new_buys; sells = new_sells}} in
    Some (t, "hit")
  else None

(* TODO: Fix this so it doesn't have circular dependencies.  *)
(*
let make_trade_stats trader transaction =
  let sell_value = transaction.bidask.bid in
  let buy_value = transaction.bidask.ask in
  let inv = trader.inventory in 
  let book = trader.orderbook.transactions in
  let avg_val = trader.avg_buy_value in
  if trader.orderbook.buys = 0 then
    let new_buys = trader.orderbook.buys + 1 in
    let new_sells = trader.orderbook.sells in 
    let new_cash = trader.cash - buy_value in
    let newtransaction = {
      transaction with order_type = "bid"
    } in
    let t = {trader with avg_buy_value = 
                           (avg_val* (new_buys - 1) + buy_value) / new_buys; 
                         profit = (get_curr_profit new_cash sell_value inv+1); 
                         inventory = inv - 1; 
                         orderbook = {transactions = newtransaction::book; 
                                      buys = new_buys; sells = new_sells}} in
    Some (t, "hit")
  else if trader.orderbook.buys < 3 then 
    if sell_value > avg_val && inv > 0 then
      let new_buys = trader.orderbook.buys in
      let new_sells = trader.orderbook.sells + 1 in 
      let new_cash = trader.cash + sell_value in
      let newtransaction = {
        transaction with order_type = "ask"
      } in
      let t = {trader with profit = (get_curr_profit new_cash sell_value inv-1); 
                           inventory = inv + 1; 
                           orderbook = {transactions = newtransaction::book; 
                                        buys = new_buys; sells = new_sells}} in 
      Some (t, "lift")
    else if buy_value < avg_val && trader.cash > (buy_value * 2) then 
      let new_buys = trader.orderbook.buys + 1 in
      let new_sells = trader.orderbook.sells in 
      let new_cash = trader.cash - buy_value in
      let newtransaction = {
        transaction with order_type = "bid"
      } in
      let t = {trader with avg_buy_value = (avg_val + buy_value) / new_buys; 
                           profit = (get_curr_profit new_cash sell_value inv+1); 
                           inventory = inv - 1; 
                           orderbook = {transactions = newtransaction::book; 
                                        buys = new_buys; sells = new_sells}} in
      Some (t, "hit")
    else None
  else
    let trans_list = trader.orderbook.transactions in
    let list = get_bids trans_list in 
    let least_sr = Stats.last_three_lsr list in 
    if (Pervasives.float sell_value) > least_sr && inv > 0 then
      let new_buys = trader.orderbook.buys in
      let new_sells = trader.orderbook.sells + 1 in 
      let new_cash = trader.cash + sell_value in
      let newtransaction = {
        transaction with order_type = "ask"
      } in
      let t = {trader with profit = (get_curr_profit new_cash sell_value inv-1); 
                           inventory = inv + 1; 
                           orderbook = {transactions = newtransaction::book; 
                                        buys = new_buys; sells = new_sells}} in 
      Some (t, "lift")
    else if (Pervasives.float buy_value) < least_sr && trader.cash > 
    (buy_value * 2) then 
      let new_buys = trader.orderbook.buys + 1 in
      let new_sells = trader.orderbook.sells in 
      let new_cash = trader.cash - buy_value in
      let newtransaction = {
        transaction with order_type = "bid"
      } in
      let t = {trader with avg_buy_value = (avg_val + buy_value) / new_buys; 
                           profit = (get_curr_profit new_cash sell_value inv+1); 
                           inventory = inv - 1; 
                           orderbook = {transactions = newtransaction::book; 
                                        buys = new_buys; sells = new_sells}} in
      Some (t, "hit")
    else None *)



(**[get_final_profit trader] is the profit of the trader type t at the end 
   of the game. *)
let get_final_profit trader =
  trader.profit + (trader.inventory * trader.hidden_number)


(** [make_trade_ai0 trader transaction] returns the trade outcome
    for the AI #0 trader, [trader], with trade [transaction]. *)
let make_trade_ai0 (trader:t) (transaction:transaction) = 
  make_trade_dumb trader transaction

(** [make_trade_ai1 trader transaction] returns the trade outcome
    for the AI #1 trader, [trader], with trade [transaction]. *)
let make_trade_ai1 (trader:t) (transaction:transaction) = 
  make_trade_optimist trader transaction

(** [make_trade_ai2 trader transaction] returns the trade outcome
    for the AI #2 trader, [trader], with trade [transaction]. *)
let make_trade_ai2 (trader:t) (transaction:transaction) = 
  make_trade_weary trader transaction


(** [contention_for_trade traders_data trans] will return an option indicating a 
    single transaction, indicating the outcome for the market maker's bid/ask. 
    It randomly choses a trader from all traders who are willing to transact.
    If no traders want to transact, it returns None. 
*)
let contention_for_trade (traders_data : trader_players) (trans :transaction) = 
  let response1 = make_trade_ai0 traders_data.simple_ai trans in 
  let response2 = make_trade_ai1 traders_data.ai1 trans in 
  let response3 = make_trade_ai2 traders_data.ai2 trans in 
  let agg = response1::response2::response3::[] in
  (* print_string (string_of_int (List.length agg)); *)
  let candidates = List.filter (
      fun x -> match x with
        | None -> false
        | Some (t, response)  -> true
    ) agg in
  if (List.length candidates = 0) then None else 
    let winner_index = Random.int (List.length candidates) in
    List.nth candidates winner_index