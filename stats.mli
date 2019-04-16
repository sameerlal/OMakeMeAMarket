type graph_data = {bid_data : int list; ask_data : int list; trade_data : int list; time_data : int list; true_value : int}


val to_float_list : string list -> float list
val get_mean : float list -> float
val get_variance : float list -> float
val last_three_lsr : int list -> float
val get_graph : Marketmaker.t -> Trader.t -> graph_data 