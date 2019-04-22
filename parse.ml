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



(**[situationofjson json] is a type situation with event and effect from the 
   [json] file. *)
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
    situations = json |> member "situation" |> to_list |> List.map situationsofjson;
    ascii = json |> member "ascii" |> to_string;
  }

(* Obtain a question from json *)
(**[get_intro data] results in the ascii string being printed from the json. *)
let get_intro (data: t) =
  print_endline (data.ascii)


(**[get_question_struct data] is a type fermiquestions from type t [data]*)
let get_question_struct (data : t) = 
  List.nth data.fermi (int_of_string data.id)

let get_question (data:t) : string = 
  let fermi_struct = get_question_struct data in 
  fermi_struct.question

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



