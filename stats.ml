open String
open Trader
open Marketmaker

type graph_data = {bid_data : int list; ask_data : int list; trade_data : int list; time_data : int list; true_value : int}

(**[to_float_list_acc acc] is a list of floats from a string list. *)
let rec to_float_list_acc acc = function
  |[] -> acc
  |h::t -> to_float_list_acc ((float_of_string h):: acc) t 

(**[to_float_list lst] is a list of floats converted from the string list [lst] *)
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
  |[] -> failwith "empty list"
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

let plot_data data =
  failwith "unimplemented"

let rec get_data true_val bidask_lst bids asks trades times =
  match bidask_lst with
  | [] -> {bid_data = bids; ask_data = asks; trade_data = trades; time_data = times; true_value = true_val}
  | h::t -> get_data true_val t (h.bid::bids) (h.ask::asks) (h.trade_type::trades) times

let get_graph (market:Marketmaker.t) (trader:Trader.t) =
  let true_val = trader.true_value in
  let bidask_lst = market.bid_ask_history in
  let times = market.timestamp in 
  let bid_lst = [] in 
  let ask_lst = [] in 
  let trade = [] in 
  get_data true_val bidask_lst bid_lst ask_lst trade times


