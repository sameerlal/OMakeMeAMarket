type object_phrase = string list
type command = 
  | Set of object_phrase  | Inventory | History | First_Trade | Last_Trade | Help | Quit | Tutorial
exception Empty
exception Malformed

val parse: string -> command