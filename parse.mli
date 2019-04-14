type t

val from_json : Yojson.Basic.json -> t
val get_question : t -> string
val get_answer : t -> string
val introduction : 'a -> unit