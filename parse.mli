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