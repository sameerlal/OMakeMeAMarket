type object_phrase = string list

type command = 
  | Set of object_phrase   
  | Inventory  
  | History 
  | Help  
  | Quit  (*TODO  *)
  | Tutorial
  | Cheat

exception Empty
exception Malformed

val parse: string -> command