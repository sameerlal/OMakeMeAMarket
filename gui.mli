(** Gui

    Representation of the tutorial provided to the player before the game is 
    played.

    This module contains the print statements and situations that can allow a 
    player to understand how to play the game. It has structs that represent 
    undervaluing or overvaluing of a security and gives feedback to the player 
    based on the bid/ask prices that they have set. The tutorial simulates the 
    actions that traders might make based on those prices and gives a general 
    idea of what the right actions would be. *)

val preamble: unit -> unit

val introduction: unit -> unit
val introduction_adv: unit -> unit
val display_help : 'a -> unit
val tutorial_preamble: 'a -> unit