open Coursedata
open OUnit2
open Makeschedule

let rec to_timeslots (m: meeting_time list) : timeslot list =
  match m with
  | [] -> []
  | h::t ->
    [{shorthand = ""; kind = Lec; course_number = 0; meeting_times = [h];
      course_shorthand = ""}]@(to_timeslots t)

let five_meetings_no_conflict : meeting_time list = [
  (Mon, 100, 200);
  (Tue, 100, 200);
  (Wed, 100, 200);
  (Thu, 100, 200);
  (Fri, 100, 200);]

let five_meetings_one_conflict : meeting_time list = [
  (Mon, 100, 200);
  (Tue, 100, 200);
  (Wed, 100, 200);
  (Tue, 100, 200);
  (Fri, 100, 200);]

let make_is_doublebooked_test title f = 
  "make_schedule.is_doublebooked: " ^ title >:: f

let is_doublebooked_tests = [
  make_is_doublebooked_test "two disjoint meeting times work" begin fun _ -> 
    assert_bool ""  (not(is_doublebooked [(Mon, 100, 200); (Mon,500, 600)]))
  end;

  make_is_doublebooked_test "two meeting times on different days works" begin 
    fun _ -> 
      assert_bool ""  (not(is_doublebooked [(Mon, 100, 200); (Tue, 100, 200)]))
  end;

  make_is_doublebooked_test 
    "the beginning of one overlaps with the end of another" begin fun _ -> 
    assert_bool "" (is_doublebooked [(Mon, 100, 200); (Mon, 150, 250)])
  end;

  make_is_doublebooked_test 
    "the end of one meeting time overlaps with the start of another" begin 
    fun _ -> 
      assert_bool "" (is_doublebooked [(Mon, 100, 200); (Mon, 50, 150)])
  end;

  make_is_doublebooked_test "a meeting time is inside another" begin fun _ -> 
    assert_bool "" (is_doublebooked [(Mon, 100, 200); (Mon, 125, 175)])
  end;

  make_is_doublebooked_test "a meeting time is outside another" begin fun _ -> 
    assert_bool "" (is_doublebooked [(Mon, 100, 200); (Mon, 50, 250)])
  end;

  make_is_doublebooked_test "a meeting time is exactly another" begin fun _ -> 
    assert_bool "" (is_doublebooked [(Mon, 100, 200); (Mon, 100, 200)])
  end;

  make_is_doublebooked_test "five disjoint meeting times" begin fun _ -> 
    assert_bool "" (not(is_doublebooked five_meetings_no_conflict))
  end;

  make_is_doublebooked_test "five meetings with one conflict" begin fun _ -> 
    assert_bool "" (is_doublebooked five_meetings_one_conflict)
  end;
]

let make_where_doublebooked_test title f =
  "make_schedule.where_doublebooked: " ^ title >:: f

let where_doublebooked_tests = [
  make_where_doublebooked_test "two disjoint meeting times work" begin fun _ -> 
    assert_equal (-1)
      (where_doublebooked (to_timeslots [(Mon, 100, 200); (Mon,500, 600)]))
  end;

  make_where_doublebooked_test "two meeting times on different days works" begin 
    fun _ -> assert_equal (-1)
        (where_doublebooked (to_timeslots [(Mon, 100, 200); (Tue, 100, 200)]))
  end;

  make_where_doublebooked_test 
    "the beginning of one overlaps with the end of another" begin fun _ -> 
    assert_equal 1 (where_doublebooked
                      (to_timeslots [(Mon, 100, 200); (Mon, 150, 250)]))
  end;

  make_where_doublebooked_test 
    "the end of one meeting time overlaps with the start of another" begin 
    fun _ -> 
      assert_equal 1 (where_doublebooked
                        (to_timeslots [(Mon, 100, 200); (Mon, 50, 150)]))
  end;

  make_where_doublebooked_test "a meeting time is inside another" begin fun _ -> 
    assert_equal 1 (where_doublebooked
                      (to_timeslots [(Mon, 100, 200); (Mon, 125, 175)]))
  end;

  make_where_doublebooked_test "a meeting time is outside another" begin fun _ -> 
    assert_equal 1 (where_doublebooked
                      (to_timeslots [(Mon, 100, 200); (Mon, 50, 250)]))
  end;

  make_where_doublebooked_test "a meeting time is exactly another" begin fun _ -> 
    assert_equal 1 (where_doublebooked
                      (to_timeslots [(Mon, 100, 200); (Mon, 100, 200)]))
  end;

  make_where_doublebooked_test "five disjoint meeting times" begin fun _ -> 
    assert_equal (-1)
      (where_doublebooked (to_timeslots five_meetings_no_conflict))
  end;

  make_where_doublebooked_test "five meetings with one conflict" begin fun _ -> 
    assert_equal 3
      (where_doublebooked (to_timeslots five_meetings_one_conflict))
  end;
]

let discussion_conflict_json = Yojson.Basic.from_string {|
[
  {
    "subject": "CS",
    "catalogNbr": "1110",
    "titleShort": "Intro Computing Using Python",
    "enrollGroups": [
      {
        "classSections": [
          {
            "ssrComponent": "LEC",
            "section": "001",
            "classNbr": 10791,
            "meetings": [
              {
                "timeStart": "09:05AM",
                "timeEnd": "09:55AM",
                "instructors": [
                  {}
                ],
                "pattern": "TR"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "DIS",
            "section": "201",
            "classNbr": 10793,
            "meetings": [
              {
                "timeStart": "12:20PM",
                "timeEnd": "01:10PM",
                "instructors": [
                  {}
                ],
                "pattern": "T"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          }
        ],
        "unitsMaximum": 4
      }
    ],
    "description": "Programming and problem solving using Python. Emphasizes principles of software development, style, and testing. Topics include procedures and functions, iteration, recusion, arrays and vectors, strings, an operational model of procedure and function calls, algorithms, exceptions, object-oriented programming, and GUIs (graphical user interfaces). Weekly labs provide guided practice on the computer, with staff present to help. Assignments use graphics and GUIs to help develop fluency and understanding."
  },
  {
    "subject": "CS",
    "catalogNbr": "1112",
    "titleShort": "Intro Computing Using MATLAB",
    "enrollGroups": [
      {
        "classSections": [
          {
            "ssrComponent": "LEC",
            "section": "002",
            "classNbr": 10779,
            "meetings": [
              {
                "timeStart": "11:15AM",
                "timeEnd": "12:05PM",
                "instructors": [
                  {}
                ],
                "pattern": "TR"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "DIS",
            "section": "201",
            "classNbr": 10780,
            "meetings": [
              {
                "timeStart": "12:20PM",
                "timeEnd": "01:10PM",
                "instructors": [
                  {}
                ],
                "pattern": "T"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "DIS",
            "section": "202",
            "classNbr": 10781,
            "meetings": [
              {
                "timeStart": "01:25PM",
                "timeEnd": "02:15PM",
                "instructors": [
                  {}
                ],
                "pattern": "T"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          }
        ],
        "unitsMaximum": 4
      }
    ],
    "description": "Programming and problem solving using MATLAB. Emphasizes the systematic development of algorithms and programs. Topics include iteration, functions, arrays, recursion, object-oriented programming, and MATLAB graphics. Assignments are designed to build an appreciation for complexity, dimension, fuzzy data, inexact arithmetic, randomness, simulation, and the role of approximation."
  }
]
|}

let discussion_conflict_course_list = Coursedata.from_json
    discussion_conflict_json

let make_discussion_conflict_test title f =
  "make_schedule.discussion_conflict: " ^ title >:: f

let discussion_conflict_tests = [  
  make_discussion_conflict_test "CS 1110 LEC 001 is included" begin fun _ -> 
    let timeslots = make_schedule discussion_conflict_course_list in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1110" &&
        timeslot.shorthand = "LEC 001") timeslots);
  end;

  make_discussion_conflict_test "CS 1110 DIS 201 is included" begin fun _ -> 
    let timeslots = make_schedule discussion_conflict_course_list in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1110" &&
        timeslot.shorthand = "DIS 201") timeslots);
  end;

  make_discussion_conflict_test "CS 1112 LEC 002 is included" begin fun _ -> 
    let timeslots = make_schedule discussion_conflict_course_list in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "LEC 002") timeslots);
  end;

  make_discussion_conflict_test "CS 1112 DIS 201 is not included" begin fun _ -> 
    let timeslots = make_schedule discussion_conflict_course_list in
    assert_bool "" (not(List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "DIS 201") timeslots));
  end;

  make_discussion_conflict_test "CS 1112 DIS 202 is included" begin fun _ -> 
    let timeslots = make_schedule discussion_conflict_course_list in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "DIS 202") timeslots);
  end;
]

let lecture_conflict_json = Yojson.Basic.from_string {|
[
  {
    "subject": "CS",
    "catalogNbr": "1110",
    "titleShort": "Intro Computing Using Python",
    "enrollGroups": [
      {
        "classSections": [
          {
            "ssrComponent": "LEC",
            "section": "001",
            "classNbr": 10791,
            "meetings": [
              {
                "timeStart": "11:15AM",
                "timeEnd": "12:05PM",
                "instructors": [
                  {}
                ],
                "pattern": "TR"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "DIS",
            "section": "201",
            "classNbr": 10793,
            "meetings": [
              {
                "timeStart": "12:20PM",
                "timeEnd": "01:10PM",
                "instructors": [
                  {}
                ],
                "pattern": "F"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          }
        ],
        "unitsMaximum": 4
      }
    ],
    "description": "Programming and problem solving using Python. Emphasizes principles of software development, style, and testing. Topics include procedures and functions, iteration, recusion, arrays and vectors, strings, an operational model of procedure and function calls, algorithms, exceptions, object-oriented programming, and GUIs (graphical user interfaces). Weekly labs provide guided practice on the computer, with staff present to help. Assignments use graphics and GUIs to help develop fluency and understanding."
  },
  {
    "subject": "CS",
    "catalogNbr": "1112",
    "titleShort": "Intro Computing Using MATLAB",
    "enrollGroups": [
      {
        "classSections": [
          {
            "ssrComponent": "LEC",
            "section": "002",
            "classNbr": 10779,
            "meetings": [
              {
                "timeStart": "11:15AM",
                "timeEnd": "12:05PM",
                "instructors": [
                  {}
                ],
                "pattern": "TR"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "LEC",
            "section": "003",
            "classNbr": 10779,
            "meetings": [
              {
                "timeStart": "11:15PM",
                "timeEnd": "11:45PM",
                "instructors": [
                  {}
                ],
                "pattern": "S"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "DIS",
            "section": "201",
            "classNbr": 10780,
            "meetings": [
              {
                "timeStart": "12:20PM",
                "timeEnd": "01:10PM",
                "instructors": [
                  {}
                ],
                "pattern": "W"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "DIS",
            "section": "202",
            "classNbr": 10781,
            "meetings": [
              {
                "timeStart": "01:25PM",
                "timeEnd": "02:15PM",
                "instructors": [
                  {}
                ],
                "pattern": "T"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          }
        ],
        "unitsMaximum": 4
      }
    ],
    "description": "Programming and problem solving using MATLAB. Emphasizes the systematic development of algorithms and programs. Topics include iteration, functions, arrays, recursion, object-oriented programming, and MATLAB graphics. Assignments are designed to build an appreciation for complexity, dimension, fuzzy data, inexact arithmetic, randomness, simulation, and the role of approximation."
  }
]
|}

let lecture_conflict_course_list = Coursedata.from_json lecture_conflict_json

let make_lecture_conflict_test title f =
  "make_schedule.lecture_conflict: " ^ title >:: f

let lecture_conflict_tests = [  
  make_lecture_conflict_test "CS 1110 LEC 001 is included" begin fun _ -> 
    let timeslots = make_schedule lecture_conflict_course_list in
    assert_bool "" (List.exists (fun timeslot -> 
        timeslot.course_shorthand = "CS 1110" && timeslot.shorthand = "LEC 001") 
        timeslots);
  end;
  make_lecture_conflict_test "CS 1112 LEC 003 is included" begin fun _ -> 
    let timeslots = make_schedule lecture_conflict_course_list in
    assert_bool "" (List.exists (fun timeslot -> 
        timeslot.course_shorthand = "CS 1112" && timeslot.shorthand = "LEC 003") 
        timeslots);
  end;
  make_lecture_conflict_test "CS 1112 DIS 201 is included" begin fun _ -> 
    let timeslots = make_schedule lecture_conflict_course_list in
    assert_bool "" (List.exists (fun timeslot -> 
        timeslot.course_shorthand = "CS 1112" && timeslot.shorthand = "DIS 201") 
        timeslots);
  end;
  make_lecture_conflict_test "CS 1110 DIS 201 is included" begin fun _ -> 
    let timeslots = make_schedule lecture_conflict_course_list in
    assert_bool "" (List.exists (fun timeslot -> 
        timeslot.course_shorthand = "CS 1110" && timeslot.shorthand = "DIS 201") 
        timeslots);
  end;
]

let lecture_conflict_json = Yojson.Basic.from_string {|
[
  {
    "subject": "CS",
    "catalogNbr": "1110",
    "titleShort": "Intro Computing Using Python",
    "enrollGroups": [
      {
        "classSections": [
          {
            "ssrComponent": "LEC",
            "section": "001",
            "classNbr": 10791,
            "meetings": [
              {
                "timeStart": "11:15AM",
                "timeEnd": "12:05PM",
                "instructors": [
                  {}
                ],
                "pattern": "TR"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "DIS",
            "section": "201",
            "classNbr": 10793,
            "meetings": [
              {
                "timeStart": "12:20PM",
                "timeEnd": "01:10PM",
                "instructors": [
                  {}
                ],
                "pattern": "F"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          }
        ],
        "unitsMaximum": 4
      }
    ],
    "description": "Programming and problem solving using Python. Emphasizes principles of software development, style, and testing. Topics include procedures and functions, iteration, recusion, arrays and vectors, strings, an operational model of procedure and function calls, algorithms, exceptions, object-oriented programming, and GUIs (graphical user interfaces). Weekly labs provide guided practice on the computer, with staff present to help. Assignments use graphics and GUIs to help develop fluency and understanding."
  },
  {
    "subject": "CS",
    "catalogNbr": "1112",
    "titleShort": "Intro Computing Using MATLAB",
    "enrollGroups": [
      {
        "classSections": [
          {
            "ssrComponent": "LEC",
            "section": "002",
            "classNbr": 10779,
            "meetings": [
              {
                "timeStart": "11:15AM",
                "timeEnd": "12:05PM",
                "instructors": [
                  {}
                ],
                "pattern": "TR"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "LEC",
            "section": "003",
            "classNbr": 10779,
            "meetings": [
              {
                "timeStart": "11:15PM",
                "timeEnd": "11:45PM",
                "instructors": [
                  {}
                ],
                "pattern": "S"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "DIS",
            "section": "201",
            "classNbr": 10780,
            "meetings": [
              {
                "timeStart": "12:20PM",
                "timeEnd": "01:10PM",
                "instructors": [
                  {}
                ],
                "pattern": "W"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "DIS",
            "section": "202",
            "classNbr": 10781,
            "meetings": [
              {
                "timeStart": "01:25PM",
                "timeEnd": "02:15PM",
                "instructors": [
                  {}
                ],
                "pattern": "T"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          }
        ],
        "unitsMaximum": 4
      }
    ],
    "description": "Programming and problem solving using MATLAB. Emphasizes the systematic development of algorithms and programs. Topics include iteration, functions, arrays, recursion, object-oriented programming, and MATLAB graphics. Assignments are designed to build an appreciation for complexity, dimension, fuzzy data, inexact arithmetic, randomness, simulation, and the role of approximation."
  }
]
|}

let lecture_conflict_course_list = Coursedata.from_json lecture_conflict_json

let make_lecture_conflict_test title f =
  "make_schedule.lecture_conflict: " ^ title >:: f

let lecture_conflict_tests = [  
  make_lecture_conflict_test "CS 1110 LEC 001 and DIS 201 are included" begin
    fun _ -> 
      let timeslots = make_schedule lecture_conflict_course_list in
      assert_bool "" ((List.exists (fun timeslot ->
          timeslot.course_shorthand = "CS 1110" &&
          timeslot.shorthand = "LEC 001") timeslots) &&
                      (List.exists (fun timeslot ->
                           timeslot.course_shorthand = "CS 1110" &&
                           timeslot.shorthand = "DIS 201") timeslots))
  end;

  make_lecture_conflict_test "CS 1112 LEC 002 is not included" begin fun _ -> 
    let timeslots = make_schedule lecture_conflict_course_list in
    assert_bool "" (not(List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "LEC 002") timeslots));
  end;

  make_lecture_conflict_test "CS 1112 LEC 003 is included" begin fun _ -> 
    let timeslots = make_schedule lecture_conflict_course_list in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "LEC 003") timeslots);
  end;

  make_lecture_conflict_test "CS 1112 DIS 201 is not included" begin fun _ -> 
    let timeslots = make_schedule lecture_conflict_course_list in
    assert_bool "" (not(List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "DIS 201") timeslots));
  end;

  make_lecture_conflict_test "CS 1112 DIS 202 is included" begin fun _ -> 
    let timeslots = make_schedule lecture_conflict_course_list in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "DIS 202") timeslots);
  end;
]

let lab_conflict_json = Yojson.Basic.from_string {|
[
  {
    "subject": "CS",
    "catalogNbr": "1110",
    "titleShort": "Intro Computing Using Python",
    "enrollGroups": [
      {
        "classSections": [
          {
            "ssrComponent": "LEC",
            "section": "001",
            "classNbr": 10791,
            "meetings": [
              {
                "timeStart": "11:15AM",
                "timeEnd": "12:05PM",
                "instructors": [
                  {}
                ],
                "pattern": "TR"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "DIS",
            "section": "201",
            "classNbr": 10793,
            "meetings": [
              {
                "timeStart": "12:20PM",
                "timeEnd": "01:10PM",
                "instructors": [
                  {}
                ],
                "pattern": "F"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "LAB",
            "section": "001",
            "classNbr": 10791,
            "meetings": [
              {
                "timeStart": "11:15AM",
                "timeEnd": "12:05PM",
                "instructors": [
                  {}
                ],
                "pattern": "MW"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          }
        ],
        "unitsMaximum": 4
      }
    ],
    "description": "Programming and problem solving using Python. Emphasizes principles of software development, style, and testing. Topics include procedures and functions, iteration, recusion, arrays and vectors, strings, an operational model of procedure and function calls, algorithms, exceptions, object-oriented programming, and GUIs (graphical user interfaces). Weekly labs provide guided practice on the computer, with staff present to help. Assignments use graphics and GUIs to help develop fluency and understanding."
  },
  {
    "subject": "CS",
    "catalogNbr": "1112",
    "titleShort": "Intro Computing Using MATLAB",
    "enrollGroups": [
      {
        "classSections": [
          {
            "ssrComponent": "LEC",
            "section": "002",
            "classNbr": 10779,
            "meetings": [
              {
                "timeStart": "11:15AM",
                "timeEnd": "12:05PM",
                "instructors": [
                  {}
                ],
                "pattern": "TR"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "LEC",
            "section": "003",
            "classNbr": 10779,
            "meetings": [
              {
                "timeStart": "11:15PM",
                "timeEnd": "11:45PM",
                "instructors": [
                  {}
                ],
                "pattern": "S"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "DIS",
            "section": "201",
            "classNbr": 10780,
            "meetings": [
              {
                "timeStart": "12:20PM",
                "timeEnd": "01:10PM",
                "instructors": [
                  {}
                ],
                "pattern": "W"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "DIS",
            "section": "202",
            "classNbr": 10781,
            "meetings": [
              {
                "timeStart": "01:25PM",
                "timeEnd": "02:15PM",
                "instructors": [
                  {}
                ],
                "pattern": "T"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "LAB",
            "section": "002",
            "classNbr": 10780,
            "meetings": [
              {
                "timeStart": "11:15AM",
                "timeEnd": "12:05PM",
                "instructors": [
                  {}
                ],
                "pattern": "MW"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "LAB",
            "section": "003",
            "classNbr": 10780,
            "meetings": [
              {
                "timeStart": "07:30PM",
                "timeEnd": "08:30PM",
                "instructors": [
                  {}
                ],
                "pattern": "MW"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          }
        ],
        "unitsMaximum": 4
      }
    ],
    "description": "Programming and problem solving using MATLAB. Emphasizes the systematic development of algorithms and programs. Topics include iteration, functions, arrays, recursion, object-oriented programming, and MATLAB graphics. Assignments are designed to build an appreciation for complexity, dimension, fuzzy data, inexact arithmetic, randomness, simulation, and the role of approximation."
  }
]
|}

let lab_conflict_course_list = Coursedata.from_json lab_conflict_json

let make_lab_conflict_test title f = 
  "make_schedule.lab_conflict: " ^ title >:: f

let lab_conflict_tests = [
  make_lab_conflict_test "CS 1110 LEC 001 is included" begin fun _ -> 
    let timeslots = make_schedule lab_conflict_course_list in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1110" &&
        timeslot.shorthand = "LEC 001") timeslots);
  end;

  make_lab_conflict_test "CS 1110 DIS 201 is included" begin fun _ -> 
    let timeslots = make_schedule lab_conflict_course_list in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1110" &&
        timeslot.shorthand = "DIS 201") timeslots);
  end;

  make_lab_conflict_test "CS 1110 LAB 001 is included" begin fun _ -> 
    let timeslots = make_schedule lab_conflict_course_list in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1110" &&
        timeslot.shorthand = "LAB 001") timeslots);
  end;

  make_lab_conflict_test "CS 1112 LEC 002 is not included" begin fun _ -> 
    let timeslots = make_schedule lab_conflict_course_list in
    assert_bool "" (not(List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "LEC 002") timeslots));
  end;

  make_lab_conflict_test "CS 1112 LEC 003 is included" begin fun _ -> 
    let timeslots = make_schedule lab_conflict_course_list in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "LEC 003") timeslots);
  end;

  make_lab_conflict_test "CS 1112 DIS 201 is not included" begin fun _ -> 
    let timeslots = make_schedule lab_conflict_course_list in
    assert_bool "" (not(List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "DIS 201") timeslots));
  end;

  make_lab_conflict_test "CS 1112 DIS 202 is included" begin fun _ -> 
    let timeslots = make_schedule lab_conflict_course_list in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "DIS 202") timeslots);
  end;

  make_lab_conflict_test "CS 1112 LAB 002 is not included" begin fun _ -> 
    let timeslots = make_schedule lab_conflict_course_list in
    assert_bool "" (not(List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "LAB 002") timeslots));
  end;

  make_lab_conflict_test "CS 1112 LAB 003 is included" begin fun _ -> 
    let timeslots = make_schedule lab_conflict_course_list in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "LAB 003") timeslots);
  end;
]

let lec_dis_conflict_json = Yojson.Basic.from_string {|
[
  {
    "subject": "CS",
    "catalogNbr": "1110",
    "titleShort": "Intro Computing Using Python",
    "enrollGroups": [
      {
        "classSections": [
          {
            "ssrComponent": "LEC",
            "section": "001",
            "classNbr": 10791,
            "meetings": [
              {
                "timeStart": "11:15AM",
                "timeEnd": "12:05PM",
                "instructors": [
                  {}
                ],
                "pattern": "TR"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "DIS",
            "section": "201",
            "classNbr": 10793,
            "meetings": [
              {
                "timeStart": "12:20PM",
                "timeEnd": "01:10PM",
                "instructors": [
                  {}
                ],
                "pattern": "F"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          }
        ],
        "unitsMaximum": 4
      }
    ],
    "description": "Programming and problem solving using Python. Emphasizes principles of software development, style, and testing. Topics include procedures and functions, iteration, recusion, arrays and vectors, strings, an operational model of procedure and function calls, algorithms, exceptions, object-oriented programming, and GUIs (graphical user interfaces). Weekly labs provide guided practice on the computer, with staff present to help. Assignments use graphics and GUIs to help develop fluency and understanding."
  },
  {
    "subject": "CS",
    "catalogNbr": "1112",
    "titleShort": "Intro Computing Using MATLAB",
    "enrollGroups": [
      {
        "classSections": [
          {
            "ssrComponent": "LEC",
            "section": "002",
            "classNbr": 10779,
            "meetings": [
              {
                "timeStart": "11:15AM",
                "timeEnd": "12:05PM",
                "instructors": [
                  {}
                ],
                "pattern": "TR"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "LEC",
            "section": "003",
            "classNbr": 10779,
            "meetings": [
              {
                "timeStart": "11:15PM",
                "timeEnd": "11:45PM",
                "instructors": [
                  {}
                ],
                "pattern": "S"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "DIS",
            "section": "204",
            "classNbr": 10780,
            "meetings": [
              {
                "timeStart": "10:25AM",
                "timeEnd": "11:15AM",
                "instructors": [
                  {}
                ],
                "pattern": "T"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "DIS",
            "section": "205",
            "classNbr": 10781,
            "meetings": [
              {
                "timeStart": "01:25PM",
                "timeEnd": "02:15PM",
                "instructors": [
                  {}
                ],
                "pattern": "T"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          }
        ],
        "unitsMaximum": 4
      }
    ],
    "description": "Programming and problem solving using MATLAB. Emphasizes the systematic development of algorithms and programs. Topics include iteration, functions, arrays, recursion, object-oriented programming, and MATLAB graphics. Assignments are designed to build an appreciation for complexity, dimension, fuzzy data, inexact arithmetic, randomness, simulation, and the role of approximation."
  }
]
|}

let lec_dis_conflict_course_list = Coursedata.from_json lec_dis_conflict_json

let make_lec_dis_conflict_test title f =
  "make_schedule.lec_dis_conflict: " ^ title >:: f

let lec_dis_conflict_tests = [
  make_lec_dis_conflict_test "CS 1110 LEC 001 is included" begin fun _ -> 
    let timeslots = make_schedule lec_dis_conflict_course_list in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1110" &&
        timeslot.shorthand = "LEC 001") timeslots);
  end;

  make_lec_dis_conflict_test "CS 1110 DIS 201 is included" begin fun _ -> 
    let timeslots = make_schedule lec_dis_conflict_course_list in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1110" &&
        timeslot.shorthand = "DIS 201") timeslots);
  end;

  make_lec_dis_conflict_test "CS 1112 LEC 002 is not included" begin fun _ -> 
    let timeslots = make_schedule lec_dis_conflict_course_list in
    assert_bool "" (not(List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "LEC 002") timeslots));
  end;

  make_lec_dis_conflict_test "CS 1112 LEC 003 is included" begin fun _ -> 
    let timeslots = make_schedule lec_dis_conflict_course_list in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "LEC 003") timeslots);
  end;

  make_lec_dis_conflict_test "CS 1112 DIS 204 is not included" begin fun _ -> 
    let timeslots = make_schedule lec_dis_conflict_course_list in
    assert_bool "" (not(List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "DIS 204") timeslots));
  end;

  make_lec_dis_conflict_test "CS 1112 DIS 205 is included" begin fun _ -> 
    let timeslots = make_schedule lec_dis_conflict_course_list in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "DIS 205") timeslots);
  end;
]

let make_courses_conflict_json = Yojson.Basic.from_string {|
[
  {
    "subject": "CS",
    "catalogNbr": "1110",
    "titleShort": "Intro Computing Using Python",
    "enrollGroups": [
      {
        "classSections": [
          {
            "ssrComponent": "LEC",
            "section": "001",
            "classNbr": 10791,
            "meetings": [
              {
                "timeStart": "11:15AM",
                "timeEnd": "12:05PM",
                "instructors": [
                  {}
                ],
                "pattern": "TR"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "DIS",
            "section": "201",
            "classNbr": 10793,
            "meetings": [
              {
                "timeStart": "12:20PM",
                "timeEnd": "01:10PM",
                "instructors": [
                  {}
                ],
                "pattern": "F"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          }
        ],
        "unitsMaximum": 4
      }
    ],
    "description": "Programming and problem solving using Python. Emphasizes principles of software development, style, and testing. Topics include procedures and functions, iteration, recusion, arrays and vectors, strings, an operational model of procedure and function calls, algorithms, exceptions, object-oriented programming, and GUIs (graphical user interfaces). Weekly labs provide guided practice on the computer, with staff present to help. Assignments use graphics and GUIs to help develop fluency and understanding."
  },
  {
    "subject": "CS",
    "catalogNbr": "1113",
    "titleShort": "Intro Fake Computing Using Fake Language",
    "enrollGroups": [
      {
        "classSections": [
          {
            "ssrComponent": "LEC",
            "section": "001",
            "classNbr": 10791,
            "meetings": [
              {
                "timeStart": "11:15AM",
                "timeEnd": "12:05PM",
                "instructors": [
                  {}
                ],
                "pattern": "TR"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "DIS",
            "section": "201",
            "classNbr": 10793,
            "meetings": [
              {
                "timeStart": "12:20PM",
                "timeEnd": "01:10PM",
                "instructors": [
                  {}
                ],
                "pattern": "F"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          }
        ],
        "unitsMaximum": 4
      }
    ],
    "description": "Programming and problem solving using Python. Emphasizes principles of software development, style, and testing. Topics include procedures and functions, iteration, recusion, arrays and vectors, strings, an operational model of procedure and function calls, algorithms, exceptions, object-oriented programming, and GUIs (graphical user interfaces). Weekly labs provide guided practice on the computer, with staff present to help. Assignments use graphics and GUIs to help develop fluency and understanding."
  },
  {
    "subject": "CS",
    "catalogNbr": "1112",
    "titleShort": "Intro Computing Using MATLAB",
    "enrollGroups": [
      {
        "classSections": [
          {
            "ssrComponent": "LEC",
            "section": "002",
            "classNbr": 10779,
            "meetings": [
              {
                "timeStart": "11:15AM",
                "timeEnd": "12:05PM",
                "instructors": [
                  {}
                ],
                "pattern": "TR"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "LEC",
            "section": "003",
            "classNbr": 10779,
            "meetings": [
              {
                "timeStart": "11:15PM",
                "timeEnd": "11:45PM",
                "instructors": [
                  {}
                ],
                "pattern": "S"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "DIS",
            "section": "202",
            "classNbr": 10780,
            "meetings": [
              {
                "timeStart": "10:25AM",
                "timeEnd": "11:15AM",
                "instructors": [
                  {}
                ],
                "pattern": "T"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "DIS",
            "section": "203",
            "classNbr": 10781,
            "meetings": [
              {
                "timeStart": "01:25PM",
                "timeEnd": "02:15PM",
                "instructors": [
                  {}
                ],
                "pattern": "T"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          }
        ],
        "unitsMaximum": 4
      }
    ],
    "description": "Programming and problem solving using MATLAB. Emphasizes the systematic development of algorithms and programs. Topics include iteration, functions, arrays, recursion, object-oriented programming, and MATLAB graphics. Assignments are designed to build an appreciation for complexity, dimension, fuzzy data, inexact arithmetic, randomness, simulation, and the role of approximation."
  }
]
|}

let make_courses_conflict_course_list =
  Coursedata.from_json make_courses_conflict_json


let make_make_courses_test title f = 
  "make_schedule.make_courses: " ^ title >:: f

(** Order:
    0. CS 1112
    1. CS 1113
    2. CS 1110*)

let p1 = {course = List.nth make_courses_conflict_course_list 2;
          required = true;
          priority = 0}

let p2 = {course = List.nth make_courses_conflict_course_list 2;
          required = false;
          priority = 10}

let p3 = {course = List.nth make_courses_conflict_course_list 1;
          required = true;
          priority = 0}

let p4 = {course = List.nth make_courses_conflict_course_list 1;
          required = false;
          priority = 3}

let p5 = {course = List.nth make_courses_conflict_course_list 0;
          required = true;
          priority = 0}

let p6 = {course = List.nth make_courses_conflict_course_list 0;
          required = false;
          priority = 5}

let required_only = [p1;p5]

let no_required = [p2;p6]

let required_conflict = [p1;p3]

let not_req_conflict = [p2;p4]

let req_not_req_conflict = [p1;p4]

let make_courses_tests = [

  (* Tests for two required preferences with solvable conflicts *)
  make_make_courses_test "CS 1110 LEC 001 is included" begin fun _ -> 
    let timeslots = make_courses required_only in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1110" &&
        timeslot.shorthand = "LEC 001") timeslots);
  end;

  make_make_courses_test "CS 1110 DIS 201 is included" begin fun _ -> 
    let timeslots = make_courses required_only in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1110" &&
        timeslot.shorthand = "DIS 201") timeslots);
  end;

  make_make_courses_test "CS 1112 LEC 002 is not included" begin fun _ -> 
    let timeslots = make_courses required_only in
    assert_bool "" (not(List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "LEC 002") timeslots));
  end;

  make_make_courses_test "CS 1112 LEC 003 is included" begin fun _ -> 
    let timeslots = make_courses required_only in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "LEC 003") timeslots);
  end;

  make_make_courses_test "CS 1112 DIS 202 is not included" begin fun _ -> 
    let timeslots = make_courses required_only in
    assert_bool "" (not(List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "DIS 202") timeslots));
  end;

  make_make_courses_test "CS 1112 DIS 203 is included" begin fun _ -> 
    let timeslots = make_courses required_only in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "DIS 203") timeslots);
  end;

  (* Tests for two non-required preferences with solvable conflicts*)
  make_make_courses_test "CS 1110 LEC 001 is included" begin fun _ -> 
    let timeslots = make_courses no_required in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1110" &&
        timeslot.shorthand = "LEC 001") timeslots);
  end;

  make_make_courses_test "CS 1110 DIS 201 is included" begin fun _ -> 
    let timeslots = make_courses no_required in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1110" &&
        timeslot.shorthand = "DIS 201") timeslots);
  end;

  make_make_courses_test "CS 1112 LEC 002 is not included" begin fun _ -> 
    let timeslots = make_courses no_required in
    assert_bool "" (not(List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "LEC 002") timeslots));
  end;

  make_make_courses_test "CS 1112 LEC 003 is included" begin fun _ -> 
    let timeslots = make_courses no_required in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "LEC 003") timeslots);
  end;

  make_make_courses_test "CS 1112 DIS 202 is not included" begin fun _ -> 
    let timeslots = make_courses no_required in
    assert_bool "" (not(List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "DIS 202") timeslots));
  end;

  make_make_courses_test "CS 1112 DIS 203 is included" begin fun _ -> 
    let timeslots = make_courses no_required in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1112" &&
        timeslot.shorthand = "DIS 203") timeslots);
  end;
  (* Tests for two required preferences with no possible schedules*)
  make_make_courses_test "Exception was raised" begin fun _ -> 
    let timeslots () = make_courses required_conflict in
    assert_raises (No_schedule_possible) (timeslots);
  end;

  (* Tests two non-required preferences with an unsolvable conflict*)
  make_make_courses_test "CS 1110 LEC 001 is included" begin fun _ -> 
    let timeslots = make_courses not_req_conflict in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1110" &&
        timeslot.shorthand = "LEC 001") timeslots);
  end;

  make_make_courses_test "CS 1110 DIS 201 is included" begin fun _ -> 
    let timeslots = make_courses not_req_conflict in
    assert_bool "" (List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1110" &&
        timeslot.shorthand = "DIS 201") timeslots);
  end;

  make_make_courses_test "CS 1113 LEC 001 is not included" begin fun _ -> 
    let timeslots = make_courses not_req_conflict in
    assert_bool "" (not(List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1113" &&
        timeslot.shorthand = "LEC 001") timeslots));
  end;

  make_make_courses_test "CS 1110 DIS 201 is not included" begin fun _ -> 
    let timeslots = make_courses not_req_conflict in
    assert_bool "" (not(List.exists (fun timeslot ->
        timeslot.course_shorthand = "CS 1113" &&
        timeslot.shorthand = "DIS 201") timeslots));
  end;
]

let tests = List.flatten [
    is_doublebooked_tests;
    where_doublebooked_tests;
    discussion_conflict_tests;
    lecture_conflict_tests;
    lec_dis_conflict_tests;
    lab_conflict_tests;
    make_courses_tests;
  ]