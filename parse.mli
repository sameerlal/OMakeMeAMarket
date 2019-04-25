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

(**[from_json json] is a type t with values from the [json] file. *)
val from_json : Yojson.Basic.t -> t

(** [get_question data] returns a question from the [data] variant *)
val get_question : t -> string

(** [get_answer data] returns an answer from the [data] variant *)
val get_answer : t -> string

(* [get_nth_situation index data] is the nth situation from json. *)
val get_nth_situation : int -> t -> situation

(* [get_event_from_situation sit] is the event from a situation.  *)
val get_event_from_situation: situation -> string

(* [get_effect_from_situation sit] is the effect from a situation. It's an 
   option indicating if it it causes the price to rise or to fall *)
val get_effect_from_situation : situation -> (string * string) option

(**[get_intro data] results in the ascii introduction string being printed from 
   the json. *)
val get_intro : t -> unit

(** [show_dice n] returns a string indicating a dice with visible face [n]. *)
val show_dice : int -> string