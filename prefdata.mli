open Coursedata
open Yojson.Basic
open Makeschedule

val prefs_from_json : json -> course list -> preference list 