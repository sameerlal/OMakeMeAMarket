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

(**[init_trader hidden identifier] initalizes a trader record with hidden number 
   [hidden] and id equal to [identifier] *)
val init_trader :  int -> string -> t

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
val make_trade : t -> transaction -> (t * string) option

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
val make_trade_dumb : t -> transaction -> (t * string) option

(**[make_transaction timestamp bid ask order_type] returns a variant transaction
   that takes in a [timestamp], [order_type] and a [bid] and [ask]. *)
val make_transaction : int -> int -> int -> string -> transaction

(** [contention_for_trade traders_data trans] will return an option indicating a 
    single transaction, indicating the outcome for the market maker's bid/ask. 
    It randomly choses a trader from all traders who are willing to transact.
    If no traders want to transact, it returns None. 
*)
val contention_for_trade: trader_players -> transaction -> (t * string) option