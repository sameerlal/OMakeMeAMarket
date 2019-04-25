(** Parse
    Representation of the game's intro.

    This module initializes the game by extracting from the relevant json file 
    and providing the player with fermiquestions/diceroll. It parses the json 
    and introduces the situations that can occur in the json.
*)
type t
type situation = {
  event : string;
  effect : string;
}

(** These are for testing purposes that is all: BEGIN *)
val fermi_json : t
val oth_situation : situation
val first_situation : situation
(* END *)
val from_json : Yojson.Basic.t -> t
val get_question : t -> string
val get_answer : t -> string
val get_nth_situation : int -> t -> situation
val get_event_from_situation: situation -> string
val get_effect_from_situation : situation -> (string * string) option
val get_intro : t -> unit
val show_dice : int -> string