
(** The abstract type of values representing adventures. *)
type t
type receive_transaction
val init_market : string -> t
val get_timestamp : t -> int
val transaction : receive_transaction -> t -> t
val generate_receive_transaction : int -> string -> int -> int -> receive_transaction