type graph_data = {bid_data : int list; ask_data : int list; 
                   trade_data : string list; time_data : int list; 
                   hidden_number : int}

type dice_data = {
  player_roll : int;
  other_rolls : int list;
  sum_rolls : int;
}

val mega_roll : int -> dice_data
val get_player_roll : dice_data -> int
val to_float_list : string list -> float list
val get_mean : float list -> float
val get_variance : float list -> float
val last_three_lsr : int list -> float
val get_graph : Marketmaker.t -> Trader.t -> graph_data 
val text_capture : Marketmaker.t -> unit
val linear_reg_cheat : Marketmaker.t -> dice_data -> float
val chebyshevs_var : float -> float