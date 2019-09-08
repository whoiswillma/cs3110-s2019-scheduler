open Coursedata

type schedule_style = No_style | Early | Late | Spread

type preference = {course : course; required : bool; priority : int}

exception No_schedule_possible

(**  
 * [is_doublebooked meeting_times] is true iff there are two meetings that occur
 * at the same time on the same day of the week. 
*)
val is_doublebooked : meeting_time list -> bool 

val where_doublebooked : timeslot list -> int

(** 
 * [make_schedule ~schedule_style:style ~progress:p courses] produces a schedule
 * using the courses [coureses]. Progress is called every 5% with the current 
 * percentage as the argument. [schedule_style] optimizes the schedule based on 
 * what the style is. 
 *
 * If a schedule could not be formed, then a [No_schedule_possible] exception
 * is thrown.
*)
val make_schedule : 
  ?schedule_style:schedule_style -> 
  ?progress:(int -> unit) -> 
  course list ->  
  timeslot list

(** [make_courses p] takes in a preference list p and returns the best possible
    schedule given preference list p
*)
val make_courses :
  ?schedule_style: schedule_style -> 
  ?progress: (int -> unit) -> 
  preference list -> timeslot list
