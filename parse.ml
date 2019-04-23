open Yojson.Basic.Util
open String


type situation = {
  event : string;
  effect : string;
}


type fermiquestions = 
  {question : string;
   answer : string;
  }


type t = {
  id: string;
  fermi : fermiquestions list;
  situations : situation list;
  ascii : string
}
(* This is for testing purposes BEGIN *)
let fermi_json : t = {id = "1";
                      fermi = [{question = "What is the volume of air that I breathe in one day?"; answer = "2800"};
                               {question = "How many people in the world are talking on their cell phones in any given minute? (in millions)"; answer = "125"}];
                      situations = [{event = "Flash Crash, all stocks are down"; effect = "-10%"};
                                    {event = "Interest rates decrease"; effect = "+2%"}];
                      ascii =
                        "
WW      WW EEEEEEE LL       CCCCC   OOOOO  MM    MM EEEEEEE          TTTTTTT RRRRRR    AAA   DDDDD   EEEEEEE RRRRRR     !!! 
WW      WW EE      LL      CC    C OO   OO MMM  MMM EE                 TTT   RR   RR  AAAAA  DD  DD  EE      RR   RR    !!! 
WW   W  WW EEEEE   LL      CC      OO   OO MM MM MM EEEEE              TTT   RRRRRR  AA   AA DD   DD EEEEE   RRRRRR     !!! 
 WW WWW WW EE      LL      CC    C OO   OO MM    MM EE                 TTT   RR  RR  AAAAAAA DD   DD EE      RR  RR         
  WW   WW  EEEEEEE LLLLLLL  CCCCC   OOOO0  MM    MM EEEEEEE            TTT   RR   RR AA   AA DDDDDD  EEEEEEE RR   RR    !!!
      "  }

let oth_situation = {event = "Flash Crash, all stocks are down"; effect = "-10%"}
let first_situation = {event = "Interest rates decrease"; effect = "+2%"}
(* END *)



(**[situationofjson json] stores into a record a situation with event and 
   effect from the [json] file. *)
let situationsofjson json = 
  {
    event = json |> member "event" |> to_string ;
    effect = json |> member "effect" |> to_string ;
  }

(**[fermisofjson json] is a type fermiquestions with question and answer from 
   the [json] file. *)
let fermisofjson json = 
  {
    question = json |> member "question" |> to_string ;
    answer = json |> member "answer" |> to_string ;
  }

(**[from_json json] is a type t with values from the [json] file. *)
let from_json json = 
  {
    id = json |> member "id" |> to_string;
    fermi = json |> member "fermi" |> to_list |> List.map fermisofjson;
    situations = json |> member "situation" |> to_list |> 
                 List.map situationsofjson;
    ascii = json |> member "ascii" |> to_string;
  }

(**[get_intro data] results in the ascii introduction string being printed from 
   the json. *)
let get_intro (data: t) =
  print_endline (data.ascii)


(**[get_question_struct data] is a type fermiquestions from type t [data]*)
let get_question_struct (data : t) = 
  List.nth data.fermi (int_of_string data.id)

(** [get_question data] returns a question from the [data] variant *)
let get_question (data:t) : string = 
  let fermi_struct = get_question_struct data in 
  fermi_struct.question

(** [get_answer data] returns an answer from the [data] variant *)
let get_answer (data : t) : string = 
  let fermi_struct = get_question_struct data in 
  fermi_struct.answer

(* [get_nth_situation index data] is the nth situation from json. *)
let get_nth_situation (index: int ) (data : t) =
  List.nth data.situations index

(* [get_event_from_situation sit] is the event from a situation.  *)
let get_event_from_situation (sit : situation) = 
  sit.event

(* [get_effect_from_situation sit] is the effect from a situation. It's an option indicating
   if it it causes the price to rise or to fall *)
let get_effect_from_situation (sit: situation) = 
  let s = (String.get sit.effect 0) in
  let rest = (String.sub sit.effect 1 (String.length sit.effect - 1)) in 
  match s with
  | '+' -> Some ("increase", rest)
  | '-' -> Some ("decrease", rest) (*decrease?*)
  | _ -> failwith "Error with situation's effect value in json"




(** [show_1] returns a string indicating a dice with visible face "1". *)
let show_1 = 
  "
     _____________
    |             |
    |             |
    |             |   
    |             |
    |      O      |
    |             |
    |             |
    |             |
    |_____________|
    "
(** [show_2] returns a string indicating a dice with visible face "2". *)
let show_2 = 
  "
     _____________
    |             |
    |   O         |
    |             |   
    |             |
    |             |
    |             |
    |             |
    |           O |
    |_____________|
    "
(** [show_3] returns a string indicating a dice with visible face "3". *)
let show_3 = 
  "
     _____________
    |             |
    |  O          |
    |             |
    |             |
    |      O      |
    |             |
    |             |
    |           O |
    |_____________|
    "

(** [show_4] returns a string indicating a dice with visible face "4". *)
let show_4 = 
  "
     _____________
    |             |
    |             |
    |   O    O    |
    |             |
    |             |
    |             |   
    |   O     O   |
    |             |
    |_____________|
    "

(** [show_5] returns a string indicating a dice with visible face "5". *)
let show_5 = 
  "
     _____________
    |             |
    |   O     O   |
    |             |
    |             |
    |      O      |
    |             |
    |             |   
    |   O     O   |
    |_____________|
    "
(** [show_6] returns a string indicating a dice with visible face "6". *)
let show_6 = 
  "
     _____________
    |             |
    |             |
    |   O  O  O   |
    |             |
    |             |
    |             |   
    |   O  O  O   |
    |             |
    |_____________|
    "
(** [show_0] returns a string indicating a dice with visible face "?". *)
let show_0 = 
  "
     __________        __________           __________ 
    |          |      |          |         |          |
    |          |      |          |         |          |
    |    ??    |      |    ??    |         |    ??    |
    |          |      |          |         |          |
    |          |      |          |         |          |
    |__________|      |__________|         |__________|
    "
(** [show_dice n] returns a string indicating a dice with visible face [n]. *)
let show_dice (n:int) : string = 
  match n with
  | 0 -> show_0
  | 1 -> show_1
  | 2 -> show_2
  | 3 -> show_3
  | 4 -> show_4
  | 5 -> show_5
  | 6 -> show_6
  | _ -> failwith "out of bounds dice"