type object_phrase = string list
type command = 
  | Profit
  | Set of object_phrase 
  | Inventory
  | Orderbook
  | First_Trade
  | Last_Trade
  | Quit
exception Empty
exception Malformed

val parse: string -> command