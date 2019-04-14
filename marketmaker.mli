
(** The abstract type of values representing adventures. *)
type t
type receive_transaction
val init_market : string -> t
val get_timestamp : t -> int
val transaction : receive_transaction -> t -> t
val generate_receive_transaction : int -> string -> int -> int -> receive_transaction
val stringify_bid_ask : t -> string
val get_outstandingshares : t -> int
val increment_timestep : t -> t