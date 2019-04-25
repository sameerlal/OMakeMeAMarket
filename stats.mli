(** Stats

    Representation of the statistics of actions performed in-game.

    This module uses the data gathered from the player's actions during the 
    course of the game and their interactions with the traders to provide them 
    with statistics that might aid in future decision-making.
*)

(*Holds the data about a player's transactions and bid/ask prices. *)
type graph_data = {bid_data : int list; ask_data : int list; 
                   trade_data : string list; time_data : int list; 
                   hidden_number : int}

(*Holds a player's dice roll data. *)
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
val chebyshev_like_var : float -> float