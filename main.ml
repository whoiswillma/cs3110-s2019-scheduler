open Makeschedule
open Coursedata
open Yojson.Basic.Util
open ANSITerminal
open Prefdata

(**Min-max a schedule to your preference. *)

(**Type for text that will be printed on the terminal.*)
type text =
  | Word of string * (style list)
  | Compound of (text list) * text
  | Newline
  | Blank

type settings = {
  pinned:timeslot list;
  blacklist:timeslot list;
  schedule_style:schedule_style;
  (* max_credits:int option; *)
}

type state = {
  title: string;
  data:course list;
  preferences:preference list; 
  settings:settings;
  commands:command list;
}
and command = {
  name:string;
  aliases:string list;
  arg_names:string list;
  description:string;
  func:(state -> string list -> state * text);
}

type validity =
  | Valid of command
  | Invalid of string
  | Empty

(* BEGIN HELPER FUNCTIONS *)
(**[hor_border ()] returns a string that contains enough repeated characters
   of '-' to fill the width of the terminal*)
let hor_border () =
  let term_width = fst (size ()) in
  String.make term_width '-'

(**[sum_credits sched] returns the total amount of course credits in a 
   schedule *)
let sum_credits (sched:timeslot list) (data:course list)=
  let get_course (ts:timeslot) = match course_with_shorthand ts.course_shorthand
                                         data with
  | None -> failwith (ts.course_shorthand ^ " not found")
  | Some c -> c
  in
  let course_list_dups = List.map get_course sched in 
  let course_list = List.sort_uniq compare course_list_dups in
  List.fold_left (fun acc cour -> acc + cour.credits) 0 course_list

(**[cmp_timeslot ts1 ts2 compares two timeslots in a similar wat to 
   Pervasives.compare] *)
let cmp_timeslot (ts1:timeslot) (ts2:timeslot) : int =
  let cmp_course = compare ts1.course_shorthand ts2.course_shorthand in
  let shorthand_to_int (t:timeslot) =
    match t.kind with
    | Lec -> 2
    | Dis -> 1
    | Lab -> 0
  in
  if cmp_course = 0 then
    compare (shorthand_to_int ts1) (shorthand_to_int ts2)
  else cmp_course

(**[meeting_time_to_string t] creates a formatted string for printing time *)
let meeting_time_to_string (t:meeting_time) : string =
  let pad (i:int) : string =
    let i_string = string_of_int i in
    if i < 10 then "0" ^ i_string else i_string in
  let min_to_clock (m:int) : string =
    let hour = pad (match (m/60) mod 12 with |0 -> 12 |n -> n) in
    let min = pad (m mod 60) in
    let am_pm = if m < 720 then "AM" else "PM" in
    hour ^ ":" ^ min ^ am_pm 
  in
  let dow_to_string (d:dow) : string =
    match d with
    | Sun -> "Sun"
    | Mon -> "Mon"
    | Tue -> "Tue"
    | Wed -> "Wed"
    | Thu -> "Thu"
    | Fri -> "Fri"
    | Sat -> "Sat"
  in
  match t with d,s,e -> (dow_to_string d) ^ " " 
                        ^ (min_to_clock s) ^ "-" ^ (min_to_clock e)

(**[courses_to_strings c_ts_list] returns a list of strings to be sequentially
   printed *)
let courses_to_strings (c_ts_list:(course*(timeslot list)) list) : string list =
  let sort_ts c_ts = List.sort cmp_timeslot c_ts in
  let timeslot_to_string (ts:timeslot) =
    "--" ^ ts.shorthand ^ ": "
    ^ String.concat " | " (List.rev (List.map meeting_time_to_string 
                                       ts.meeting_times))
  in
  let rec timeslots_to_strings (ts_list:timeslot list) (acc:string list) =
    match ts_list with
    | [] -> acc
    | h::t -> timeslots_to_strings t ((timeslot_to_string h)::acc)
  in 
  let map_c_ts_to_strings = function
    | c, ts_l -> (c.shorthand^":") :: timeslots_to_strings (sort_ts ts_l) []
  in
  List.flatten (List.map map_c_ts_to_strings c_ts_list)

(**[schedule_to_string sched] returns a string that contains a 
   rudementary overview of a schedule.*)
let schedule_to_string (sched:timeslot list) (data:course list) : string =
  let intro_lines = [
    hor_border();
    "total credits: " ^ string_of_int (sum_credits sched data);
    hor_border();
  ] in 
  let course_lines = courses_to_strings (make_course_timeslots data sched) in
  String.concat "\n" (intro_lines @ course_lines)

(**[match_course data inpit] returns a course option with a corresponding string
   to print if a course with shorthand [input] was found in [data] or not*)
let match_course (data:course list) (input:string) : course option * string =
  let exp = Str.regexp_string_case_fold input in
  let rec contains_input acc = function
    | [] -> acc
    | h :: t ->
      if Str.string_match exp (h.shorthand) 0 then contains_input (h :: acc) t
      else contains_input acc t
  in
  let course_shorthands = List.map (fun c -> c.shorthand) 
      (contains_input [] data) in 
  match course_shorthands with
  | [] -> None, (input ^ " not in roster")
  | [c] -> if c = input 
    then course_with_shorthand input data, c
    else None, "Did you mean: " ^ c ^ "?"
  | list -> None, "Did you mean: " ^ String.concat " | " list ^ "?"

(**[find_course data input] returns a course option with a corresponding string
   to print if a course matching keywords from [input] was found in [data] or not*)
let find_course (data:course list) (input:string) : course option * string =
  let filter = String.split_on_char '-' input in
  let keywd = List.hd (List.rev filter) in
  let dept = List.hd filter in
  let no_dept = keywd = String.trim keywd && dept = String.trim dept in
  let expstr = if no_dept && List.length filter > 1 then dept ^ "-" ^ keywd 
    else String.trim keywd in
  let exp = Str.regexp_string_case_fold (String.trim expstr) in
  let exp_dept = Str.regexp_string_case_fold 
      (String.trim (String.uppercase_ascii dept)) in
  let rec contains_input_dept acc = if not no_dept then begin function
      | [] -> acc
      | h :: t ->
        if Str.string_match exp_dept (h.shorthand) 0 
        then contains_input_dept (h :: acc) t
        else contains_input_dept acc t
    end
    else fun _ -> []
  in
  let rec contains_input_keywd (acc:course list) (d:course list)= 
    match d with
    | [] -> acc
    | hd::tl -> 
      let exn_handle = try Str.search_forward exp (hd.name) 0 with 
          Not_found -> -1 in
      if exn_handle >= 0 then contains_input_keywd (hd::acc) tl 
      else contains_input_keywd 
          acc tl
  in
  let rec combined_lst acc dept = match dept with
    | [] -> acc
    | hd::tl -> if List.mem hd (contains_input_keywd [] data) 
      then combined_lst (hd::acc) tl else combined_lst acc tl
  in
  let course_names = if not no_dept then 
      begin 
        List.rev (List.map (fun c -> c.shorthand ^ " - " ^ c.name) 
                    (combined_lst [] (contains_input_dept [] data))) 
      end
    else List.map (fun c -> c.shorthand ^ " - " ^ c.name) 
        (contains_input_keywd [] data)
  in
  match course_names with
  | [] -> None, (input ^ " yielded no matches")
  | list -> None, "Possible classes: " ^ String.concat " | " list

let prefix_concat prefix strings =
  List.fold_left (fun acc s -> acc ^ prefix ^ s) "" strings

let rec text_length = function
  | Word (s,_) -> String.length s
  | Compound (words, sep) -> 
    List.fold_left (fun acc w -> text_length w + acc) 0 words
    + (List.(length words - 1) * text_length sep)
  | Newline -> fst (size())
  | Blank -> 0

let rec print (text:text) : unit =
  (* let rec reduce (l:text list) =
     match l with
     | [] -> []
     | Blank::t -> t
     | Compound([],_)::t -> t
     | Compound(w, sep)::t -> reduce (Compound(reduce w, sep)::t)
     | e -> e
     in *)
  match text with 
  | Word (str, styles) -> printf styles "%s" str
  | Compound (words, sep) -> 
    (match words with
     | [] -> ()
     | [Compound([],_)] -> ()
     | [w] | w::[Compound([],_)] -> print w
     | Compound([],_)::t -> print (Compound(t,sep))
     | w::Compound([],_)::t -> print (Compound(w::t, sep))
     | h::t -> print h; print sep; print (Compound(t, sep)))
  | Newline -> printf [] "\n"
  | Blank -> ()

let rec fit_hor (left:text) (right:text) =
  let fill_nl l = List.init (List.length l - 1) (fun i -> Newline) in
  match left, right with
  | Newline , t -> t
  | t , Newline -> t
  | Compound(l,Newline), Compound(r,Newline) ->
    let balance l1 l2 = (match List.(length l1 - length l2) with
        | 0 -> l1, l2
        | d when d > 0 -> l1, l2 @ (List.init d (fun i -> Newline))
        | d -> l1 @ (List.init d (fun i -> Newline)), l2)
    in
    Compound((fun tup -> List.map2 fit_hor (fst tup) (snd tup))(balance l r), Newline)
  | Compound(l,Newline), r -> fit_hor left (Compound(r::fill_nl(l), Newline))
  | l, Compound(r, Newline) -> fit_hor (Compound(l::fill_nl(r), Newline)) right
  | l, r -> 
    let term_width = fst (size()) in
    let sep_length = term_width - (text_length left + text_length right) in
    Compound([left; right], Word(String.make sep_length ' ', []))

let commmands_to_text commands =
  let com_to_lines com = Word
      ("[" ^ com.name ^ (prefix_concat " " com.arg_names)
       ^ "] " ^ com.description 
      (* ^ ", alias: " ^ concat " " com.aliases *)
      , [magenta])
  in
  Compound(List.map com_to_lines commands, Newline)

let settings_to_text state =
  let sched_style_text =
    let to_str = function
      | No_style -> "Default Spread"
      | Early -> "Earliest"
      | Late -> "Latest"
      | Spread -> "Max Spread"
    in
    Word(to_str state.settings.schedule_style, [yellow; Bold])
  in
  let order = [
    sched_style_text;
    Compound([
        Word("Required courses are", [yellow]);
        Word("bold", [yellow; Bold]);
      ], Word(" ",[]));
    Compound([
        Word("Total courses:",[yellow]);
        Word(string_of_int (List.length state.preferences), [yellow; Bold]);
      ], Word(" ",[]));
  ]
  in
  Compound(order, Newline)

let prefs_to_text prefs =
  let pref_text pref = 
    let pref_style = 
      if pref.required then [blue; Bold] else [blue] in
    Compound([Word(pref.course.shorthand, pref_style); 
              Word("(" ^ (if pref.required then "R" else string_of_int pref.priority) ^ ")", [blue])], Blank)
  in
  Compound([Word("Courses: ",[blue]);
            Compound(List.map pref_text prefs, Word(", ", [blue]))], Blank)

let state_to_text (state) =
  let order = [
    Word(state.title,[Bold]);
    Blank;
    fit_hor (commmands_to_text state.commands) (settings_to_text state);
    Newline;
    prefs_to_text state.preferences; Newline;
  ] 
  in
  Compound (order, Newline)

let progress_bar (i:int) : unit =
  let size = size() in
  let bar_length = fst size - 6 in
  let bar = String.make (i*bar_length/100) '|' in
  set_cursor 1 (snd size); erase Eol;
  printf [Inverse] "%s" (bar ^ " - %" ^ string_of_int i)

let prefs_to_courses prefs = List.map (fun p -> p.course) prefs

let update_prefs state pref : state =
  let replace (f:'a->bool) (list:'a list) (e:'a) : 'a list =
    List.map (fun ele -> if f ele then e else ele) list in 
  let new_prefs = 
    if List.mem pref.course (prefs_to_courses state.preferences)
    then replace (fun p -> p.course = pref.course) state.preferences pref 
    else pref::state.preferences
  in
  {state with preferences = new_prefs}

let remove_prefs state pref : state =
  let rec remove (f:'a->bool) (list:'a list) acc : 'a list =
    match list with
    | [] -> acc
    | h::t when f h -> remove f t acc
    | h::t -> remove f t (h::acc)
  in
  let new_prefs = 
    if List.mem pref.course (prefs_to_courses state.preferences)
    then remove (fun p -> p.course = pref.course) state.preferences []
    else state.preferences
  in
  {state with preferences = new_prefs}

let args_to_course_names args =
  let exp = Str.regexp "[ ]*\\([a-zA-z]+\\)[ ]?\\([0-9]+\\)[ ]*" in
  let rec regex_courses stracc iacc str : string list =
    if Str.string_match exp str iacc
    then regex_courses (
        (Str.(
            String.uppercase_ascii (matched_group 1 str) ^ " "
            ^ matched_group 2 str))
        ::stracc)
        (Str.match_end()) str
    else stracc 
  in regex_courses [] 0 (String.concat " " args)


let match_pref data sh : preference option * string =
  match match_course data sh with 
  | None, s -> None, s
  | Some c, s-> Some {course=c; required=true; priority=0}, s

let rec search_command commands (name:string) =
  match commands, (String.lowercase_ascii name) with
  | _ , "" -> Empty
  | [], _ -> Invalid ("Unknown command: " ^ name)
  | h::t, n when h.name = n || List.mem n h.aliases -> Valid h
  | h::t, n -> search_command t n

let rec command_loop state text =
  (* Text printed before user input *)
  erase Screen; set_cursor 1 1;
  print (state_to_text state);
  print text;
  set_cursor 1 (snd (size()));
  print (Word(">", [Blink]));
  (* Ask for input, then process it and advance to next state*)
  let input = read_line() in
  let com_name, args = 
    (fun l -> List.hd l, List.tl l) (String.split_on_char ' ' input) in
  let com_validity = search_command state.commands com_name in
  match com_validity with
  | Empty -> command_loop state text
  | Invalid s -> command_loop state (Word(s, [red]))
  | Valid c ->
    (match c.func state args with
     | st, txt -> command_loop st txt)

(* END MAIN STRUCTURE *)

(* COMMAND FUNCTIONS USED IN MAIN MENU *)

let add_func state args =
  let course_names = args_to_course_names args in
  let fold_prefs acc shorthand =
    (match acc with valid_p, valid_t, invalid_t ->
     match match_pref state.data shorthand with
     | None, s -> (valid_p, valid_t, Word(s, [red])::invalid_t)
     | Some c, s -> (c::valid_p, Word(s, [green; Bold])::valid_t, invalid_t))
  in
  match List.fold_left fold_prefs ([],[],[]) course_names 
  with valid_p, valid_t, invalid_t ->
    let disp_text = 
      Compound([
          Compound([
              if List.length valid_t = 0 
              then Blank else Word("Added: ", [green]); 
              Compound(valid_t, Word(", ", [green]));
            ], Blank);
          Compound(invalid_t, Word(", ", [red]));
        ], Word(" | ", [])) 
    in
    let next_state = List.fold_left update_prefs state valid_p in
    next_state, disp_text

let remove_func state args = 
  let course_names = args_to_course_names args in
  let fold_prefs acc shorthand =
    (match acc with valid_p, valid_t, invalid_t ->
     match match_pref (prefs_to_courses state.preferences) shorthand with
     | None, s -> (valid_p, valid_t, Word(s, [red])::invalid_t)
     | Some c, s -> (c::valid_p, Word(s, [green; Bold])::valid_t, invalid_t))
  in
  match List.fold_left fold_prefs ([],[],[]) course_names 
  with valid_p, valid_t, invalid_t ->
    let disp_text = 
      Compound([
          Compound([
              if List.length valid_t = 0 
              then Blank else Word("Removed: ", [green]); 
              Compound(valid_t, Word(", ", [green]));
            ], Blank);
          Compound(invalid_t, Word(", ", [red]));
        ], Word(" | ", [])) 
    in
    let next_state = List.fold_left remove_prefs state valid_p in
    next_state, disp_text

let find_func state args =
  let cour_name = String.concat " " args in
  match find_course state.data cour_name with
  | None, s -> command_loop state (Word(s, [yellow]))
  | Some c, s -> command_loop state (Word(c.shorthand ^ " - " ^ c.name, [red]))

let make_func state args =
  let has_optional = 
    List.fold_left (fun acc p -> acc || p.required) false state.preferences in
  try
    let schedule = 
      if has_optional then
        match state.settings.schedule_style with
        | No_style -> make_courses state.preferences 
        | sty -> make_courses state.preferences
                   ~schedule_style:sty
      else
        match state.settings.schedule_style with
        | No_style -> make_schedule (prefs_to_courses state.preferences) 
        | sty -> make_schedule (prefs_to_courses state.preferences) 
    in state, Word(schedule_to_string schedule state.data, [])
  with No_schedule_possible -> state, Word("Too many conflicts", [red])

let load_func state args =
  let file_name = String.concat " " args in
  try
    let pref_json = Yojson.Basic.from_file file_name in 
    let prefs = prefs_from_json pref_json state.data in
    let next_state = List. fold_left update_prefs state prefs in
    next_state, Word("Loaded: " ^ file_name, [green])
  with 
  | Yojson.Basic.Util.Type_error _ -> 
    state, Word("Invalid file format", [red])
  | Yojson.Json_error s | Sys_error s -> state, Word(s, [red])

let desc_func state args =
  match match_course state.data (String.(concat " " args |> uppercase_ascii)) with
  | None, s -> command_loop state (Word(s, [red]))
  | Some c, s -> command_loop state 
                   (Word(c.name ^ "\nDescription: " ^ c.description, [yellow]))

let set_func state args =
  let err_text = 
    Compound([
        Compound([
            Word("Invalid syntax",[red;Bold]);
            Word("Usage:", [red])
          ], 
            Word(" - ", [red]));
        Word("set property value course (course ...)", [red])
      ], Word(" ",[]))
  in
  match List.map String.lowercase_ascii args with
  | prop::value::courses ->
    let course_names = args_to_course_names courses in
    let prefs = 
      if String.concat " " courses = "*" 
      then state.preferences 
      else
        List.fold_left 
          (fun acc name -> 
             match match_pref state.data name with
             |None, s -> acc
             |Some p, s -> p::acc 
          ) [] course_names 
    in
    (match prop with 
     |"required" -> (match value with
         |"true" -> 
           let new_prefs = List.map (fun p -> {p with required=true}) prefs in
           (List.fold_left update_prefs state new_prefs), Blank
         |"false" ->
           let new_prefs = List.map (fun p -> {p with required=false}) prefs in
           (List.fold_left update_prefs state new_prefs), Blank
         |e -> state, err_text)
     |"priority" -> 
       (try let prio = int_of_string value in
          let new_prefs = List.map (fun p -> {p with priority=prio; required = false}) prefs in
          (List.fold_left update_prefs state new_prefs), Blank
        with Failure s -> state, Word("Priority is an integer", [red]))
     | e -> state, err_text)
  | _ -> state, err_text

let reset_func state args =
  {state with 
   preferences = [];
   settings = {pinned=[]; blacklist=[]; schedule_style=No_style};
  }, Word("Reset to intial state", [green; Bold])

let style_func state args =
  let arg = String.(concat " " args |> lowercase_ascii) in
  let sty = match arg with
    | "none" | "plain" | "default" -> No_style
    | "early" | "morning" -> Early
    | "late" | "night" -> Late
    | "spread" | "lunch" | "gaps" -> Spread
    | s -> command_loop state (Compound(
        [
          Word("Possible styles: ",[yellow]);
          Compound([Word("None", [yellow; Bold]);
                    Word("Early",[yellow; Bold]);
                    Word("Late",[yellow; Bold]);
                    Word("Spread", [yellow; Bold]);
                   ], Word(" | ", [yellow]))], Blank))
  in {state with settings = {state.settings with schedule_style = sty}}, Blank

(* END COMMAND FUNCTIONS USED IN MAIN MENU *)
let init_state = {
  title = "
   ___________ _________  __  ____   _______ 
  / __/ ___/ // / __/ _ \\/ / / / /  / __/ _ \\
 _\\ \\/ /__/ _  / _// // / /_/ / /__/ _// , _/
/___/\\___/_//_/___/____/\\____/____/___/_/|_| 
";
  data = from_json (Yojson.Basic.from_file "data.json");
  preferences = [];
  settings = {
    pinned = [];
    blacklist = [];
    schedule_style = No_style;
  };
  commands = [
    {
      name = "desc";
      aliases = ["description"; "info"];
      arg_names = ["course"];
      description = "Display full name and description of a course";
      func = desc_func;
    };
    {
      name = "add";
      aliases = ["include"];
      arg_names = ["course"; "(course ...)"];
      description = "Add a course";
      func = add_func;
    };
    {
      name = "remove";
      aliases = ["delete"];
      arg_names = ["course"; "(course ...)"];
      description = "Remove a course";
      func = remove_func;
    };
    {
      name = "set";
      aliases = ["update"];
      arg_names = ["property"; "value"; "course"; "(course ...)"];
      description = "Set properties";
      func = set_func;
    };
    {
      name = "load";
      aliases = ["read"; "open"];
      arg_names = ["file"];
      description = "Load preferences from file";
      func = load_func;
    };
    {
      name = "find";
      aliases = ["search"];
      arg_names = ["(dept -)"; "course"];
      description = "Search for a course by keywords; (dept - ) is optional";
      func = find_func;
    };
    {
      name = "style";
      aliases = [];
      arg_names = ["type"; "(none, early, late, spread)"];
      description = "Set the schedule style";
      func = style_func;
    };
    {
      name = "make";
      aliases = ["print"];
      arg_names = [];
      description = "Compute and print schedule";
      func = make_func;
    };
    {
      name = "reset";
      aliases = [];
      arg_names = [];
      description = "Reset the program";
      func = reset_func;
    };
    {
      name = "quit";
      aliases = ["exit"];
      arg_names = [];
      description = "Exit the program";
      func = (fun state args -> exit 0);
    };
  ];
}

let main () =
  let welcome_msg = "Enter a list of courses to calculate a valid schedule." in
  command_loop init_state (Word(welcome_msg, [cyan]))

let () = main ()