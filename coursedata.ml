open Yojson.Basic.Util

type dow = Sun | Mon | Tue | Wed | Thu | Fri | Sat

type meeting_time = dow * int * int

type timeslot_kind = Lec | Dis | Lab

type timeslot = {
  shorthand: string;
  kind: timeslot_kind;
  course_number: int;
  meeting_times: meeting_time list;
  course_shorthand: string
}

(**
 * An internal type used to store the lectures, discussions, and labs of a class
*)
type enroll_group = {
  credits: int;
  lectures: timeslot list;
  discussions: timeslot list;
  labs: timeslot list
}

type course = { 
  name: string; 
  credits: int;
  description: string; 
  shorthand: string; 
  lectures: timeslot list;
  discussions: timeslot list;
  labs: timeslot list
}

type t = course list

(**
 * A regular expression that recognizes 12 hour times. This expression puts 
 * the hour digits in the first group, the minutes digits in the second group,
 * and "AM" or "PM" in the third.
*)
let twelve_hour_time_regex = Str.regexp 
    "\\([0-9][0-9]\\):\\([0-9][0-9]\\)\\(AM\\|PM\\)"

(**
 * [minutes_from_twelve_hour_time t] is the number of minutes since the start of 
 * the day, or [None] if a twelve-hour time could not be parsed from [t].
*)
let minutes_from_twelve_hour_time time: int option = 
  if not (Str.string_match twelve_hour_time_regex time 0) then 
    None
  else
    try
      let hours = Str.matched_group 1 time |> int_of_string in 
      let minutes = Str.matched_group 2 time |> int_of_string in 
      let period = Str.matched_group 3 time in
      Some ((if period = "AM" then 0 else 720) + 60 * (hours mod 12) + minutes)
    with 
    | Not_found | Failure _ -> None 

(** 
 * [dows_from_pattern p] is an unordered list containing [dow]s based on the 
 * pattern p. 
 * Examples: (the list may be permuted arbitrarily)
 * - [dows_from_pattern "MWF" = [Mon; Wed; Fri]]
 * - [dows_from_pattern "MTWRF" = [Mon; Tue; Wed; Thu; Fri]]
 * - [dows_from_pattern "SSu" = [Sat; Sun]]
*)
let dows_from_pattern pattern: dow list = 
  let rec chars_to_dows_acc c acc = 
    match c with
    | 'S' :: 'u' :: t -> chars_to_dows_acc t (Sun :: acc)
    | 'M' :: t -> chars_to_dows_acc t (Mon :: acc)
    | 'T' :: t -> chars_to_dows_acc t (Tue :: acc)
    | 'W' :: t -> chars_to_dows_acc t (Wed :: acc)
    | 'R' :: t -> chars_to_dows_acc t (Thu :: acc)
    | 'F' :: t -> chars_to_dows_acc t (Fri :: acc)
    | 'S' :: t -> chars_to_dows_acc t (Sat :: acc)
    | h :: t -> chars_to_dows_acc t acc
    | [] -> acc
  in
  let chars = List.init (String.length pattern) (String.get pattern) in
  chars_to_dows_acc chars []

(**
 * [meeting_times_from_json j] contains the meeting times out of the json list 
 * of meetings [j].
*)
let meeting_times_from_json json : meeting_time list = 
  json 
  |> to_list
  |> List.map begin fun json_meeting -> 
    let start_time = json_meeting |> member "timeStart" |> to_string 
                     |> minutes_from_twelve_hour_time in
    let end_time = json_meeting |> member "timeEnd" |> to_string 
                   |> minutes_from_twelve_hour_time in 
    match start_time, end_time with 
    | Some start_time, Some end_time -> 
      let dows = json_meeting |> member "pattern" |> to_string 
                 |> dows_from_pattern in
      List.map (fun dow -> (dow, start_time, end_time)) dows
    | _ -> []
  end
  |> List.flatten

(** 
 * [sections_from_json sh j] is a list of [(timeslot, kind)] for each lecture, 
 * discussion, and lab of a class. [sh] is used to set 
 * [timeslot.course_shorthand]
*)
let sections_from_json course_shorthand json: timeslot list = 
  json 
  |> to_list 
  |> List.map begin fun json_section ->
    let kind = match json_section |> member "ssrComponent" |> to_string with 
      | "LEC" -> Lec
      | "DIS" -> Dis
      | "LAB" -> Lab
      | _ -> Lec in 
    {
      shorthand = (json_section |> member "ssrComponent" |> to_string) 
                  ^ " " 
                  ^ (json_section |> member "section" |> to_string);
      kind = kind;
      course_number = json_section |> member "classNbr" |> to_int;
      meeting_times = json_section |> member "meetings" |> 
                      meeting_times_from_json;
      course_shorthand = course_shorthand
    }
  end 

(**
 * [enroll_groups_from_json sh j] are the lectures, discussions, and labs of a 
 * "enroll group". [sh] is used to set the [timeslot.course_shorthand] property.
*)
let enroll_group_from_json course_shorthand json: enroll_group = 
  match json |> to_list with 
  | [] -> { credits = 0; lectures = []; discussions = []; labs = [] }
  | h :: _ -> 
    let sections = h |> member "classSections" |> sections_from_json 
                     course_shorthand in 
    {
      credits = h |> member "unitsMaximum" |> to_int;
      lectures = List.filter (fun section -> section.kind = Lec) sections;
      discussions = List.filter (fun section -> section.kind = Dis) sections;
      labs = List.filter (fun section -> section.kind = Lab) sections;
    }

(** 
 * [course_from_json j] represents the course that a json file would  
*)
let course_from_json json: course  =
  let course_shorthand = 
    (json |> member "subject" |> to_string) ^ " " ^ (json |> member "catalogNbr"
                                                     |> to_string) in 
  let enroll_group = json |> member "enrollGroups" |> 
                     enroll_group_from_json course_shorthand in
  {
    name = json |> member "titleShort" |> to_string;
    credits = enroll_group.credits;
    description = json |> member "description" |> to_string;
    shorthand = course_shorthand;
    lectures = enroll_group.lectures;
    discussions = enroll_group.discussions; 
    labs = enroll_group.labs
  }

(**
 * Internal method used to test exceptions
*)
let from_json_unsafe json: t =
  json 
  |> to_list 
  |> List.fold_left begin fun acc json_course -> 
    course_from_json json_course :: acc
  end []

(**
 * [from_json j] are the courses stored in [j]. Courses that could not be parsed
 * are excluded from the list. If [j] could not be parsed, the empty list is 
 * returned.
*)
let from_json json: t =
  try 
    json 
    |> to_list 
    |> List.fold_left begin fun acc json_course -> 
      try 
        course_from_json json_course :: acc
      with 
      | _ -> acc
    end []
  with 
  | _ -> []

let course_with_shorthand shorthand data: course option =
  List.find_opt (fun course -> course.shorthand = shorthand) data

let replace_assoc (f: 'b -> 'b) (g: unit -> 'b) (k: 'a) (l: ('a * 'b) list): 
  ('a * 'b) list = 
  let v' = match List.assoc_opt k l with 
    | None -> g () 
    | Some v -> f v in
  (k, v') :: (List.remove_assoc k l)

let make_course_timeslots courses timeslots: (course * timeslot list) list = 
  let course_shorthand_timeslots = 
    List.fold_left (fun course_timeslots timeslot -> 
        replace_assoc (fun v -> timeslot :: v) (fun () -> [timeslot]) 
          timeslot.course_shorthand course_timeslots) [] timeslots in
  List.map (fun (course_shorthand, timeslots) -> 
      match course_with_shorthand course_shorthand courses with 
      | None -> []
      | Some c -> [c, timeslots]) course_shorthand_timeslots 
  |> List.flatten 
