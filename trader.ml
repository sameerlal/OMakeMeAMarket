open Pervasives

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
  true_value : int;
  avg_buy_value : int;
  profit : int;
  cash : int;
  inventory : int;
  orderbook : orderbook;
}

(**[init_trader unit] is a trader of type t. *)
let init_trader unit =
  {true_value = 50; avg_buy_value = 0; profit = 0; cash = 200; inventory = 0; 
   orderbook = {transactions = []; buys = 0; sells = 0}}

(**[get_curr_profit cash init_cash msell_price inventory] is the current 
   running profit of the trader. *)
let get_curr_profit cash ?(init_cash = 200) msell_price inventory =
  inventory * msell_price + cash - init_cash


let make_bidask bid ask =
  {
    bid = bid;
    ask = ask;
    spread = ask - bid;
  }

let make_transaction timestamp bid ask order_type =
  {
    timestamp = timestamp;
    bidask = (make_bidask bid ask);
    order_type = order_type;
  }

(**[make_sell trader transaction] is a pair of new type t trader (or the old 
   trader depending on whether the trader will accept the marketmaker's bid 
   for the security) and bool indicating whether trade was accepted. *)
let make_trade trader transaction =
  let sell_value = transaction.bidask.bid in
  let buy_value = transaction.bidask.ask in
  let inv = trader.inventory in 
  let book = trader.orderbook.transactions in
  let avg_val = trader.avg_buy_value in
  if trader.orderbook.buys = 0 then if buy_value < trader.true_value then
      let new_buys = trader.orderbook.buys + 1 in
      let new_sells = trader.orderbook.sells in 
      let new_cash = trader.cash + buy_value in
      let newtransaction = {
        transaction with order_type = "bid"
      } in
      let t = {trader with avg_buy_value = (avg_val + buy_value) / new_buys; 
                           profit = (get_curr_profit new_cash sell_value inv+1); 
                           inventory = inv+1; 
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
                         inventory = inv-1; 
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
                         inventory = inv+1; 
                         orderbook = {transactions = newtransaction::book; 
                                      buys = new_buys; sells = new_sells}} in
    Some (t, "hit")
  else None

(**[make_buy trader transaction] is a pair of new type t trader (or the old 
   trader depending on whether the trader will accept the marketmaker's offer 
   for the security) and bool indicating whether trade was accepted. *)

(**[get_final_profit trader] is the profit of the trader type t at the end 
   of the game. *)
let get_final_profit trader =
  trader.profit + (trader.inventory * trader.true_value)