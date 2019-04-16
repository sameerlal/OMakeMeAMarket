type t
type situation = {
  event : string;
  effect : string;
}

val from_json : Yojson.Basic.json -> t
val get_question : t -> string
val get_answer : t -> string
val get_nth_situation : int -> t -> situation
val get_event_from_situation: situation -> string
val get_effect_from_situation : situation -> (string * string) option
val get_intro : t -> unit