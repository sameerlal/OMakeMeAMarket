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

(**[mega_roll num_opp] gives dice data based on how many traders the player 
    is going against  *)
val mega_roll : int -> dice_data

(**[get_player_roll d] gives the number the player rolled *)
val get_player_roll : dice_data -> int
(**[to_float_list lst] is a list of floats converted from the string list 
   [lst] *)
val to_float_list : string list -> float list

(**[get_mean] is the mean of the values in lst. *)
val get_mean : float list -> float

(**[get_variance lst] is the variance of the values in [lst]. *)
val get_variance : float list -> float

(**[last_three_lsr lst] is the prediction of the next values based on the least 
   squares of the last three values in [lst].*)
val last_three_lsr : int list -> float

(**[get_graph market trader] is a record of the bids and asks put by the player, 
   the types of trades made and the true value of the security. It takes in 
   a [market] type t and a [trader] type t*)
val get_graph : Marketmaker.t -> Trader.t -> graph_data 

(**[text_capture market] prints out "Trace" when called  *)
val text_capture : Marketmaker.t -> unit

(**[linear_reg_cheat market] is the linear regression of the bids or asks in 
   the market based on what the market has seen more of.  *)
val linear_reg_cheat : Marketmaker.t -> dice_data -> float

(**[chebyshev_like_var var] Uses the normal variance of a float list, and
    calculates chebyshevs variance  *)
val chebyshev_like_var : float -> float