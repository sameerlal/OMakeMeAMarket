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


val init_trader :  int -> string -> t
val make_trade : t -> transaction -> (t * string) option
val make_trade_dumb : t -> transaction -> (t * string) option
val make_transaction : int -> int -> int -> string -> transaction
