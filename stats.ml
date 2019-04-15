open String

let rec to_float_list_acc acc = function
  |[] -> acc
  |h::t -> to_float_list_acc ((float_of_string h):: acc) t 
(* Typecast string list to int list *)
let to_float_list lst =
  to_float_list_acc [] lst |> List.rev

let sum lst = (List.fold_left (fun acc h -> acc +. h) 0.0 lst)

(* Calculate mean of list of integers *)
let get_mean = function
  |[] -> failwith "empty list for getting mean"
  |lst -> sum lst /. float (List.length lst)

let sum_squared lst = 
  sum (List.map (fun x -> x*.x) lst)
(* Calcuate variance of list *)
let get_variance lst = 
  (* sum of the squared values divided by length subtracted by mean squared *)
  let mean = get_mean lst in
  (sum_squared lst)/. float (List.length lst) -. (mean *. mean)
let rec last_three_val = function
  |[] -> failwith "empty list"
  |x::y::z::[] -> [float x; float y; float z]
  |h::t -> last_three_val t

(* Predict next value by least squares of last three values  *)
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




