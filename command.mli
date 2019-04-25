(** Command
    Representation of the different commands that a player can input during 
    the game.

    This module contains the list of commands that are available to a player 
    and has a function to parse the player's input into those commands that 
    the machine can recognize. *)

(*The type of the input by the player. *)
type object_phrase = string list

(*The legal commands that can be recognized. *)
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

(**[parse string] is the command type which a player wants to be acted upon. 
   It takes in a string and converts it to a type of command.
   Raises: Empty if no command given or just whitespace;
           Malformed if not a recognized command;*)
val parse: string -> command