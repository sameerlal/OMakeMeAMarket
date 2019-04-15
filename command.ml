open Pervasives

type object_phrase = string list

type command = 
  | Set of object_phrase   
  | Inventory  
  | History  (*TODO  *) 
  | First_Trade  (*TODO  *)
  | Last_Trade    (*TODO  *)
  | Help  (*TODO  *) 
  | Quit  (*TODO  *)
  | Tutorial (*TODO  *)

exception Empty

exception Malformed

(**[parse string] is the command type which a player wants to be acted upon. 
   It takes in a string and converts it to a type of command.
   Raises: Empty if no command given or just whitespace;
           Malformed if not a recognized command;*)
let parse str =
  let wordlst = String.split_on_char ' ' (String.trim str) in 
  let filtered = List.filter (fun s -> String.length s >= 1) wordlst in 
  if List.length filtered > 0 then let cmd = List.hd filtered in 
    match cmd with
    | "set" -> if List.length filtered > 0 then 
        let obj = begin match filtered with 
          | [] -> []
          | h::t -> t end
        in (Set obj) else raise Malformed
    | "inventory" -> if List.length filtered = 1 then Inventory else raise Malformed
    | "history" -> if List.length filtered = 1 then History else raise Malformed
    | "first Trade" -> if List.length filtered = 1 then First_Trade else raise Malformed
    | "last_Trade" -> if List.length filtered = 1 then Last_Trade else raise Malformed
    | "quit" -> if List.length filtered = 1 then Quit else raise Malformed
    | "tutorial" -> Tutorial
    | "help" -> Help
    | _ -> raise Malformed
  else 
    raise Empty