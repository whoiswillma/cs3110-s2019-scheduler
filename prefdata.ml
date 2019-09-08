open Coursedata
open Yojson.Basic.Util
open Makeschedule

type temp_pref = {
  course_shorthand: string;
  required: bool;
  priority: int;
}

let temp_pref_from_json json =
  { 
    course_shorthand = json |> member "name" |> to_string;
    required = begin 
      match json |> member "required" |> to_bool_option with 
      | None -> false 
      | Some b -> b end;
    priority = begin
      match json |> member "priority" |> to_int_option with 
      |  None -> 0
      | Some i -> i end
  }

let prefs_from_json json courses = 
  json 
  |> member "courses" 
  |> to_list 
  |> List.map temp_pref_from_json 
  |> List.map (fun temp_pref ->
      match course_with_shorthand temp_pref.course_shorthand courses with 
      | None -> []
      | Some c -> [{
          course = c;
          required = temp_pref.required;
          priority = temp_pref.priority;
        }])
  |> List.flatten