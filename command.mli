type object_phrase = string list
type command = 
  | Profit | Set of object_phrase  | Inventory | Orderbook | First_Trade | Last_Trade | Help | Quit | Tutorial
exception Empty
exception Malformed

val parse: string -> command