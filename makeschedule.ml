open Coursedata

type schedule_style = No_style | Early | Late | Spread

type preference = {course : course; required : bool; priority : int}

type weighted_bool_list = {courses : bool list; weight : int}

exception No_schedule_possible

(**  
 * [index_satisfying p l] is the index of the first element of [l] that 
 * satsifies [p], or -1 if such an element does not exist.
*)
let index_satisfying p l = 
  let rec index_satisfying_i p l i =
    match l with 
    | [] -> -1
    | h :: t -> 
      if p h then i 
      else index_satisfying_i p t (i + 1)
  in 
  index_satisfying_i p l 0 

(**
 * [early_where_doublebooked timeslots] returns the first index in [timeslots]
 * where a doublebooking occurs, i.e. the first timeslot that conflicts with a 
 * timeslot later on.
*)
let early_where_doublebooked (timeslots: timeslot list) =
  let rec where_doublebooked_slice 
      (prev: timeslot list) 
      (next: timeslot list) = 
    match next with 
    | [] -> -1
    | timeslot :: t ->
      let doublebook_index = index_satisfying (fun prev_timeslot -> 
          List.exists (fun (dow, start, stop) -> 
              List.exists (fun (o_dow, o_start, o_stop) -> 
                  dow = o_dow && 
                  begin
                    (start <= o_start && o_start <= stop && stop <= o_stop)
                    || (o_stop <= stop && o_start <= start && start <= o_stop)
                    || (o_start <= start && stop <= o_stop) 
                    || (start <= o_start && o_stop <= stop)
                  end) timeslot.meeting_times)
            prev_timeslot.meeting_times
        ) prev
      in
      if doublebook_index <> -1 then doublebook_index
      else where_doublebooked_slice (timeslot :: prev) t in 
  where_doublebooked_slice [] timeslots

(**
 * [where_doublebooked timeslots] returns the last index in [timeslots] where
 * a doublebooking occurs, i.e. the first timeslot that conflicts with a 
 * previous timeslot, or -1 otherwise.
*)
let where_doublebooked (timeslots: timeslot list) =
  let rec where_doublebooked_slice 
      (prev: timeslot list) 
      (next: timeslot list)
      (i: int) = 
    match next with 
    | [] -> -1
    | timeslot :: t ->
      let is_doublebooked = List.exists (fun prev_timeslot -> 
          List.exists (fun (dow, start, stop) -> 
              List.exists (fun (o_dow, o_start, o_stop) -> 
                  dow = o_dow && 
                  begin
                    (start <= o_start && o_start <= stop && stop <= o_stop)
                    || (o_stop <= stop && o_start <= start && start <= o_stop)
                    || (o_start <= start && stop <= o_stop) 
                    || (start <= o_start && o_stop <= stop)
                  end) timeslot.meeting_times)
            prev_timeslot.meeting_times
        ) prev
      in
      if is_doublebooked then i
      else where_doublebooked_slice (timeslot :: prev) t (i + 1) in 
  where_doublebooked_slice [] timeslots 0

(**
  * [is_doublebooked meeting_times] is true iff meeting_times contains a 
  * meetingtime that overlaps with any other meeting time
*)
let is_doublebooked (meeting_times: meeting_time list): bool =
  meeting_times 
  |> List.map (fun meeting_time -> 
      { shorthand = ""; 
        kind = Lec; 
        course_number = 0; 
        meeting_times = [meeting_time]; 
        course_shorthand = "" }) 
  |> where_doublebooked |> (<>) (-1)

(**
  * [drop n l] is the list [l] with the first [n] elements removed.
*)
let rec drop n l = 
  if n <= 0 then l
  else match l with 
    | [] -> []
    | h :: t -> drop (n - 1) t

(**
 * [take n l] is the list containing only the first [n] elements of [l]
*)
let take n l = 
  let rec take_acc n l acc = 
    if n <= 0 then acc
    else match l with 
      | [] -> []
      | h :: t -> take_acc (n - 1) t (h :: acc) 
  in 
  take_acc n l [] |> List.rev

(**
 * [perm_n ll n] is the n-th permutation of the list [ll].
 * Examples:
 * - [perm_n [[1; 2; 3]; [1; 2]]], as n increases, [perm_n ll] goes
 * - [[1; 1], [1; 2], [2; 1], [2; 2], [3; 1], [3; 2]]  
*)
let perm_n ll n  =
  let rec perm_n_acc ll n acc = 
    match ll with 
    | [] -> acc 
    | h :: t -> 
      let len = List.length h in
      if len = 0 then perm_n_acc t n acc
      else perm_n_acc t (n / len) (List.nth h (n mod len) :: acc) in 
  perm_n_acc (ll |> List.rev) n []  

(**
  * [total_perms ll] is the number of permutations of the 2d-list [ll].
  * Examples:
  * - [total_perms [] = 1], since the only permutation is the empty list
  * - [total_perms [[1] = 1]
  * - [total_perms [[1; 2]] = 2]
  * - [total_perms [[1; 2]; [1; 2]] = 4]
*)
let rec total_perms = 
  List.fold_left (fun acc l -> acc * List.length l) 1

(**
 * [make_groups courses] is a list containing the lectures, discussions, and 
 * labs of each course in [courses]. [make_groups courses] does not contain 
 * any empty lists. [make_groups] will elide any lectures, discussions, or 
 * labs that contain no timeslots. Moreover, the groups will be sorted in 
 * descending order in terms of length.
*)
let make_groups courses =
  let rec make_groups_acc courses acc = 
    match courses with 
    | [] -> acc
    | h :: t -> 
      make_groups_acc t 
        ((h.lectures :: h.discussions :: h.labs :: acc) |> 
         List.filter (fun x -> List.length x <> 0)) in 
  make_groups_acc courses [] 
  |> List.sort (fun l r -> List.length l - List.length r) 

(**
  * Fold over the ints [[i, upper)] with initial value [init]. The first element
  * of the pair returned by [f] is treated as the accumulator, and the second
  * [int] value is the next index to jump to. 
*)
let rec fold_over_ints ~(i:int) ~(upper:int) ~(init:'a)
    (f: 'a -> int -> ('a * int)): 'a = 
  if i < upper then 
    let (init', i') = f init i in 
    fold_over_ints ~i:i' ~upper:upper ~init:init' f
  else init 

(** 
 * [make_schedule_weight weight progress courses] makes a schedule using 
 * weighting function [weight] and coureses [courses], and calls [progress]
 * every 5% of the coureses it updates
*)
let make_schedule_weight ?(weight: (timeslot list -> int) option) progress
    courses = 
  let groups = make_groups courses in
  let group_perms = total_perms groups in
  let five_percent_perms = group_perms / 20 in
  let schedule = fold_over_ints ~i:0 ~upper:group_perms ~init:None begin 
      fun best_so_far i -> 
        let (best_so_far', i') as result =
          let perm = perm_n groups i in
          let doublebooked_index = where_doublebooked perm in 
          if doublebooked_index <> -1 then begin 
            let jump = groups |> drop (doublebooked_index + 1) |> total_perms in
            (best_so_far, (i + jump) / jump * jump)
          end
          else 
            match best_so_far with 
            | None -> (Some perm, i + 1)
            | Some best_so_far ->
              match weight with 
              | None -> (Some perm, group_perms)
              | Some weight -> 
                if weight perm > weight best_so_far then (Some perm, i + 1)
                else (Some best_so_far, i + 1)
        in 
        begin 
          if 
            five_percent_perms <> 0 
            && i / five_percent_perms <> i' / five_percent_perms 
          then 
            progress (i' / five_percent_perms * 5)
        end;
        result
    end
  in 
  match schedule with 
  | None -> raise No_schedule_possible
  | Some s -> s 

(**
 * [split_by_dow meeting_times] is an associated list of dows and meeting times,
 * where the dow of each meeting time is the key of the associated list. 
*)
let split_by_dow meeting_times = 
  let rec split_by_dow_acc meeting_times acc = 
    match meeting_times with 
    | [] -> acc
    | (dow, _, _) as h :: t -> 
      let l = match List.assoc_opt dow acc with | None -> [] | Some l -> l in 
      split_by_dow_acc t ((dow, h :: l) :: List.remove_assoc dow acc) in
  split_by_dow_acc meeting_times []

(**
  * [shifted_pairs l] are the first two elements of l in a pair, the second
  * and third elements of l in a pair, the third and fourth element of l in a
  * pair, and so on.
*)
let shifted_pairs l = 
  List.(combine (l |> rev |> tl |> rev) (l |> tl))

let make_schedule 
    ?(schedule_style = No_style) 
    ?(progress: (int -> unit) option) 
    courses =
  let meeting_times_of_timeslots timeslots = 
    timeslots 
    |> List.map (fun timeslot -> timeslot.meeting_times) 
    |> List.flatten 
  in
  let weight = match schedule_style with 
    | No_style -> None
    | Early -> Some begin fun perm -> 
        meeting_times_of_timeslots perm
        |> List.fold_left (fun acc (_, start, _) -> acc - start) 0
      end
    | Late -> Some begin fun perm -> 
        meeting_times_of_timeslots perm
        |> List.fold_left (fun acc (_, start, _) -> acc + start) 0
      end
    | Spread -> Some begin fun perm -> 
        let dow_meeting_times = 
          perm 
          |> meeting_times_of_timeslots 
          |> split_by_dow in
        if List.length dow_meeting_times <= 1 then 0
        else
          dow_meeting_times 
          |> shifted_pairs 
          |> List.fold_left begin fun acc ((_, ll), (_, rl)) ->
            acc + abs (List.length rl - List.length ll)
          end 0
      end
  in
  let wrapped_progress = match progress with 
    | None -> fun _ -> ()
    | Some p -> p in 
  make_schedule_weight ?weight:weight wrapped_progress courses

(* MAKE_COURSES HELPER FUNCTIONS BEGIN HERE *)

(** [compare_course_list w1 w2] returns the difference between the weights of
    two course lists, w1 and w2*)
let compare_weighted_bool_lists (w1:weighted_bool_list)
    (w2:weighted_bool_list)
  : int = 
  w2.weight - w1.weight

(** [required p] returns a list of the required preferences from p*)
let rec required (p: preference list) : preference list =
  match p with
  | [] -> []
  | h::t -> if (h.required) then h::(required t) else required t

(** [not_required p] returns a list of the not-required preferences from p*)
let rec not_required (p: preference list) : preference list =
  match p with
  | [] -> []
  | h::t -> if (h.required) then not_required t else h::(not_required t)

(** [zeroes n] returns a list of falses of length n*)
let rec falses (n:int) : bool list = 
  match n with
  | 0 -> failwith "Error in Falses"
  | 1 -> [false]
  | a -> false::(falses (a-1))

(** [generate_next n] generates the next boolean permutation of *)
let rec generate_next (n:bool list) : bool list=
  match n with
  | [] -> []
  | h::t -> if (h) then false::(generate_next t) else true::t

(** [list_of_trues n] returns true if n is a list that only contains true*)
let rec list_of_trues (n:bool list) : bool = 
  match n with
  | [] -> true
  | h::t -> (h) && (list_of_trues t)

(** [binary_perms_helper n] *)
let rec binary_perms_helper (n:bool list) : (bool list) list=
  if (list_of_trues n)
  then []
  else let next = generate_next n in next::(binary_perms_helper next)

(** [generate_binary_perms n] generates a list of all binary lists of length n*)
let generate_binary_perms (n:int) : (bool list) list = 
  (falses n)::binary_perms_helper (falses n)

(** [find_weight n p] returns the weight of bool list n, given
    preferences p*)
let rec find_weight (n:bool list) (p:preference list) : int =
  match n with
  | [] -> 0
  | h::[] -> if (h) then (List.hd p).priority else 0
  | h::t -> if (h)
    then (List.hd p).priority + (find_weight t (List.tl p))
    else (find_weight t (List.tl p))

(** [find_weights n p] returns the weight of each bool list in n, given
    preferences p*)
let rec find_weights (n:(bool list) list) (p:preference list)
  : weighted_bool_list list =
  match n with
  | [] -> []
  | h::t -> {courses = h; weight = find_weight h p}::(find_weights t p)

(** [no_weights w] returns a list of the bool lists in w without the weights*)
let rec no_weights (w:weighted_bool_list list) : bool list list =
  match w with
  | [] -> []
  | {courses = c}::t -> c::(no_weights t)

(** [generate_course_lists p] returns a list of all possible int list in p,
    sorted from largest weight to smallest weight
*)
let generate_course_lists (p:preference list) : (bool list) list =
  let perms = generate_binary_perms (List.length p) in
  let weighted_courses = find_weights perms p in
  no_weights (List.sort (compare_weighted_bool_lists) weighted_courses)

(** [course_list_to_preference_list n p] returns a list of all the preferences
    in that are in int list n
*)
let rec course_list_to_preference_list (n: bool list)
    (p:preference list) : preference list =
  match n with
  | [] -> []
  | h::[] -> if (h) then [(List.hd p)] else []
  | h::t -> if (h)
    then (List.hd p)::(course_list_to_preference_list t (List.tl p))
    else (course_list_to_preference_list t (List.tl p))

(** to_courses p] returns a list of all the courses in p*)
let rec to_courses (p:preference list) : course list =
  match p with
  | [] -> []
  | h::t -> h.course::(to_courses t)

(** [try_schedules p1 n p2] returns the first possible schedule that contains
    required preferences p1 and optional preferences, n and p2*)
let rec try_schedules 
    ?(schedule_style: schedule_style option) 
    ?(progress: (int -> unit) option) 
    (p1:preference list) (n:(bool list) list)
    (p2:preference list) : timeslot list =
  match n with
  | [] -> raise No_schedule_possible 
  | h::t ->
    try 
      make_schedule
        ?schedule_style:schedule_style 
        ?progress:progress 
        (to_courses (p1@(course_list_to_preference_list h p2))) with
    | No_schedule_possible -> try_schedules p1 t p2

(* MAKE_COURSES HELPER FUNCTIONS END HERE *)

let make_courses ?(schedule_style: schedule_style option) 
    ?(progress: (int -> unit) option) (p: preference list) : timeslot list = 
  if (p = []) then [] else
    let req_list = required p in
    let not_req_list = not_required p in
    if (not_req_list = []) then
      make_schedule ?schedule_style:schedule_style 
        ?progress:progress 
        (to_courses req_list)
    else
      let course_lists = generate_course_lists not_req_list in
      try_schedules 
        ?schedule_style:schedule_style 
        ?progress:progress 
        req_list 
        course_lists 
        not_req_list