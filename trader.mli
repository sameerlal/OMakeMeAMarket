(** Trader
    Representation of the traders during the course of the game.

    This module keeps track of the market's bid/ask prices when the 
    transactions take place between the player and traders. It contains 
    structs to hold data about transactions and relevant information 
    about each trader. It has 3 types of trader and has functions to 
    implement trades for each type of trader. 
*)

(*Holds the bid/ask prices of the market. *)
type bidask = {
  bid: int;
  ask: int;
  spread: int;
}

(*Holds information regarding a transaction.*)
type transaction = {
  timestamp : int;
  bidask: bidask;
  order_type : string; (* bid or ask *)
}


(*Holds information about a particular trader's transactions and their types. *)
type orderbook = {
  transactions : transaction list;
  buys: int;
  sells: int
}

(*Holds information about each particular trader. *)
type t = {
  id : string;
  hidden_number : int;
  avg_buy_value : int;
  profit : int;
  cash : int;
  inventory : int; (* Total number of shares owned *)
  orderbook : orderbook;
}

(*The 3 types of traders. *)
type trader_players = {
  simple_ai : t;
  ai1 : t;
  ai2 : t;
}


val init_trader :  int -> string -> t
val make_trade : t -> transaction -> (t * string) option
val make_trade_dumb : t -> transaction -> (t * string) option
val make_transaction : int -> int -> int -> string -> transaction
val contention_for_trade: trader_players -> transaction -> (t * string) option