open Pervasives
open String
open Trader
open Marketmaker
(* 
  *           STATISTICS ENGINE 
  *    - Gaussian Statistics
  *    - Three point Linear Square Regression Model 
  *    - Newton-Raphson Method for Curve approximation (Secant estimate)
  *             DATA ANALYSIS
  *    - Mean spread, bids, asks, trade count
  *    - Max spread, bids, asks 
  *
  *     In implementaiton 1, the cheat command currently compares previous 
  *     bid/asks and uses a linear regression model to suggest the next move, 
  *     referencing the actual value.
  *)

(* 
 *    DICE ROLLING SIMULATION
 *     - roll dice
 *     - calculate statistics
 *)
type dice_data = {
  player_roll : int;
  other_rolls : int list;
  sum_rolls : int;
}

let rec roll_list (num:int) (acc: int list) : (int list) =
  if num = 0 
  then acc 
  else 
    roll_list (num - 1) ((1 + Random.int 5)::acc)

let get_player_roll (d:dice_data) : int = d.player_roll

let mega_roll (num_opp:int) : dice_data =
  let playroll = (Random.int 5) + 1 in 
  let other_list = roll_list num_opp [] in
  {
    player_roll = playroll;
    other_rolls = other_list;
    sum_rolls = (playroll + (List.fold_left (fun acc h -> acc + h) 0 other_list) 
                );
  }

(* END DICE *)




type graph_data = {bid_data : int list; ask_data : int list; 
                   trade_data : string list; time_data : int list;
                   hidden_number : int}

(**[to_float_list_acc acc] is a list of floats from a string list. *)
let rec to_float_list_acc acc = function
  |[] -> acc
  |h::t -> to_float_list_acc ((float_of_string h):: acc) t 

(**[to_float_list lst] is a list of floats converted from the string list 
   [lst] *)
let to_float_list lst =
  to_float_list_acc [] lst |> List.rev

(**[sum lst] is the sum of the floats in [lst] *)
let sum lst = (List.fold_left (fun acc h -> acc +. h) 0.0 lst)

(**[get_mean] is the mean of the values in lst. *)
let get_mean = function
  |[] -> failwith "empty list for getting mean"
  |lst -> sum lst /. float (List.length lst)

(**[sum_squared lst] is the sum of squares of the values in [lst]. *)
let sum_squared lst = 
  sum (List.map (fun x -> x*.x) lst)

(**[get_variance lst] is the variance of the values in [lst]. *)
let get_variance lst = 
  (* sum of the squared values divided by length subtracted by mean squared *)
  let mean = get_mean lst in
  (sum_squared lst)/. float (List.length lst) -. (mean *. mean)

(**[last_three_val] is a list of the last three values in the lst. *)
let rec last_three_val = function
  |[] -> failwith "empty list or less thasn 3 values"
  |x::y::z::[] -> [float x; float y; float z]
  |h::t -> last_three_val t

(**[last_three_lsr lst] is the prediction of the next values based on the least 
   squares of the last three values in [lst].*)
let last_three_lsr lst =
  let three = last_three_val lst in 
  let x = sum three in 
  let x2 = sum_squared three in
  let length = float (List.length lst) in
  let last_ys = [length-. 2.0; length -.1.0; length] in 
  (* make a int list to float list function *)
  let y = sum last_ys in
  let xy = List.combine three last_ys |> 
           List.fold_left (fun acc (x,y) -> acc +. (x*.y)) 0.0 in
  let m = ((length *. xy) -. (x *.y))/.((length *. x2) -. (x *. x)) in
  let b = (y -. (m *.x))/.length in
  ((length +. 1.0) -. b) /. m


(** [newton_raphson_secant f start] approximates the root of a function [f] 
    starting at seed value [start].
    Returns None if not converged or Some x where x is the converged value.  
    This approximates the derivative
    of the function as a secant line rather than the traditional tangent 
    method *)
let newton_raphson_secant f start = 
  let dfdx fu =
    fun x -> (fu (x +. 0.1) -. fu x) /. 0.1 in
  let rec iter xk number =
    let update = start -. (f xk /. (dfdx f) xk) in 
    if number > 100 then None else 
    if (abs_float (update -. xk) < 0.01) then Some xk else iter xk (number + 1) 
  in iter start 0

(**[get_max lst acc] is the max value in the list [lst]. *)
let rec get_max lst acc =
  match lst with
  | [] -> acc
  | h::t -> if h > (List.hd acc) then get_max t [h] else get_max t acc

(**[get_data true_val bidask_lst bids asks trades times] is a record 
   containing the history of the market's bids, asks, trade types and the true 
   value of 
   the security. It takes in the bidask history of the market in [bidask_lst] 
   and the [true_val] of the security. *)
let rec get_data true_val bidask_lst bids asks trades times =
  match bidask_lst with
  | [] -> {bid_data = bids; ask_data = asks; trade_data = trades; 
           time_data = times; hidden_number = true_val}
  | h::t -> get_data true_val t (h.bid::bids) (h.ask::asks) 
              (h.trade_type::trades) times

(**[list_of_ints strt nd] is a list of ints in ascending order from [strt] to 
   [nd]. *)
let rec list_of_ints strt nd lst =  
  if strt <= nd then list_of_ints (strt+1) nd (strt::lst)
  else List.rev lst

(**[get_graph market trader] is a record of the bids and asks put by the player, 
   the types of trades made and the true value of the security. It takes in 
   a [market] type t and a [trader] type t*)
let get_graph (market : Marketmaker.t) (trader : Trader.t) =
  let true_val = trader.hidden_number in
  let bidask_lst = market.bid_ask_history in
  let times = list_of_ints 0 (List.length bidask_lst) [] in 
  let bid_lst = [] in 
  let ask_lst = [] in 
  let trade = [] in 
  get_data true_val bidask_lst bid_lst ask_lst trade times

(**[bid_acc lst acc] is the list of bids in bidask list [lst]. *)
let rec bid_acc (lst : Marketmaker.bidask list) acc =
  match lst with
  | [] -> List.rev acc
  | h::t -> bid_acc t (h.bid::acc)

(**[ask_acc lst acc] is the list of asks in bidask list [lst]. *)
let rec ask_acc (lst : Marketmaker.bidask list) acc =
  match lst with
  | [] -> List.rev acc
  | h::t -> ask_acc t (h.ask::acc)

let text_capture (market : Marketmaker.t) =
  print_endline "Trace";
  ()

(**[linear_reg_cheat market] is the linear regression of the bids or asks in 
   the market based on what the market has seen more of.  *)
let linear_reg_cheat (market : Marketmaker.t ) (dice:dice_data)= 
  let bid_list = (bid_acc (market.bid_ask_history) []) in
  let ask_list = (ask_acc (market.bid_ask_history) []) in
  if List.length bid_list > List.length ask_list  then
    (* Linear regression for asks *)
    if List.length ask_list < 3 then
      -1.0  else 
      abs_float ((float_of_int dice.sum_rolls) -. 
                 (last_three_lsr 
                    ((List.nth ask_list 
                        (List.length ask_list - 3))
                     ::(List.nth ask_list 
                          (List.length ask_list - 2))
                     ::(List.nth ask_list 
                          (List.length ask_list - 1))
                     ::[])  ))
  else 
    (*  Linear regression for bids*)
  if List.length bid_list < 3 then
    -1.0  else 
    abs_float 
      ((float_of_int dice.sum_rolls) -. 
       (last_three_lsr 
          ((List.nth bid_list 
              (List.length ask_list - 3))
           ::(List.nth bid_list 
                (List.length ask_list - 2))
           ::(List.nth bid_list 
                (List.length ask_list - 1))
           ::[])  ))

(**[count lst str acc] is the frequency of occurrence of [str] in [lst]. *)
let rec count lst str acc =
  match lst with
  | [] -> acc
  | h::t -> if h = str then count t str acc+1
    else count t str acc

(**[trade_freq market trader] is the frequency of each trade type in the market. 
   It prints the output to the screen. *)
let trade_freq market trader =
  let info = get_graph market trader in 
  let trades = info.trade_data in
  let hits = count trades "hit" 0 in 
  let lifts = count trades "lift" 0 in
  let prnt = ["hits = "^(string_of_int hits); "lifts = "^(string_of_int lifts)] 
  in List.iter print_string prnt

let chebyshevs_var var =
  match var with
  |0. -> 0.
  |_ -> 1. -. (1. -. (1./.(sqrt var)))

let markhov_var a x =
  failwith ""

let matr_mul mtr lst =
  failwith ""

