type t
type orderbook
type transaction
type bidask


val init_trader :  'a -> t
val make_trade : t -> transaction -> (t * string) option
val make_transaction : int -> int -> int -> string -> transaction