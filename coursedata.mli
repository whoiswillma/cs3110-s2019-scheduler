(**
 * A day of the week
*)
type dow = Sun | Mon | Tue | Wed | Thu | Fri | Sat

(**
 * A time period during a day. [(dow, start, end)] represents a meeting on [dow] 
 * that starts from minute [start] from midnight and ends at [end] minutes from 
 * midnight. [start] must always be less than [end].
*)
type meeting_time = dow * int * int

type timeslot_kind = Lec | Dis | Lab

(**
 * A lecture, discussion, or lab of a class
*)
type timeslot = {
  shorthand: string;
  kind: timeslot_kind;
  course_number: int;
  meeting_times: meeting_time list;
  course_shorthand: string
}

(** 
 * A course offered at Cornell
*)
type course = { 
  name: string; 
  credits: int;
  description: string; 
  shorthand: string; 
  lectures: timeslot list;
  discussions: timeslot list;
  labs: timeslot list
}

(**
 * [from_json j] are the courses stored in [j]. Courses that could not be parsed
 * are excluded from the list. If [j] could not be parsed, the empty list is 
 * returned.
*)
val from_json : Yojson.Basic.json -> course list

(** 
 * [course_with_shorthand sh t] is the first course in [t] with shorthand [sh], 
 * or [None] if no such course exists. 
*)
val course_with_shorthand: string -> course list -> course option

(**
 * [make_course_timeslots course timeslots] is a list of [(course, timeslots)]
 * pairs such that the course of each timeslot in [timeslots] is [course]
*)
val make_course_timeslots : course list -> timeslot list -> 
  (course * timeslot list) list