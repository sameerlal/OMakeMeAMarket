type t
type orderbook
type transaction
type bidask


val init_trader :  int -> t
val make_trade : t -> transaction -> (t * string) option
val make_trade_dumb : t -> transaction -> (t * string) option
val make_transaction : int -> int -> int -> string -> transaction
