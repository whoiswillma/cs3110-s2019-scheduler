open OUnit2
open Coursedata 

let minimal_json = Yojson.Basic.from_string {|
[
  {
    "subject": "ABC",
    "catalogNbr": "101",
    "titleShort": "Intro to Elementary School",
    "description": "Learn your ABCs and 123s",
    "enrollGroups": [
      {
        "classSections": [
          {
            "ssrComponent": "LEC",
            "section": "001",
            "classNbr": 456,
            "meetings": [
              {
                "timeStart": "09:05AM",
                "timeEnd": "09:55AM",
                "pattern": "MTR"
              }
            ]
          },
          {
            "ssrComponent": "DIS",
            "section": "201",
            "classNbr": 789,
            "meetings": [
              {
                "timeStart": "12:00AM",
                "timeEnd": "01:00AM",
                "pattern": "SSu"
              }
            ]
          },
          {
            "ssrComponent": "LAB",
            "section": "301",
            "classNbr": 1234,
            "meetings": [
              {
                "timeStart": "11:00AM",
                "timeEnd": "01:23PM",
                "pattern": "MTR"
              }
            ]
          }
        ],
        "unitsMaximum": 4
      }
    ]
  }
]
|}

let minimal_coursedata = Coursedata.from_json minimal_json
let minimal_course = List.hd minimal_coursedata

let make_minimal_test name f = 
  "Coursedata_test.minimal: " ^ name >:: f

let minimal_tests = [
  make_minimal_test "lectures are correct" begin fun _ -> 
    let lectures = 
      [{
        shorthand = "LEC 001"; 
        kind = Lec;
        course_number = 456;
        meeting_times = [(Thu, 545, 595); (Tue, 545, 595); (Mon, 545, 595)];
        course_shorthand = "ABC 101"
      }] in 
    assert_equal lectures minimal_course.lectures 
  end;

  make_minimal_test "discussions are correct" begin fun _ -> 
    let discussions = 
      [{
        shorthand = "DIS 201"; 
        kind = Dis;
        course_number = 789;
        meeting_times = [(Sun, 0, 60); (Sat, 0, 60)];
        course_shorthand = "ABC 101"
      }] in 
    assert_equal discussions minimal_course.discussions
  end;

  make_minimal_test "labs are correct" begin fun _ -> 
    let labs = 
      [{
        shorthand = "LAB 301"; 
        kind = Lab;
        course_number = 1234;
        meeting_times = [(Thu, 660, 803); (Tue, 660, 803); (Mon, 660, 803)];
        course_shorthand = "ABC 101"
      }] in 
    assert_equal labs minimal_course.labs 
  end;

  make_minimal_test "metadata is correct" begin fun _ -> 
    assert_equal "Intro to Elementary School" minimal_course.name;
    assert_equal "Learn your ABCs and 123s" minimal_course.description;
    assert_equal "ABC 101" minimal_course.shorthand;
  end;

  make_minimal_test "credits is correct" begin fun _ -> 
    assert_equal 4 minimal_course.credits 
  end
]

let ling_json = Yojson.Basic.from_string {|
[
{
    "subject": "LING",
    "catalogNbr": "3303",
    "titleShort": "Intro to Syntax & Semantics",
    "enrollGroups": [
      {
        "classSections": [
          {
            "ssrComponent": "LEC",
            "section": "001",
            "classNbr": 5668,
            "meetings": [
              {
                "timeStart": "10:10AM",
                "timeEnd": "11:25AM",
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
            "classNbr": 5669,
            "meetings": [
              {
                "timeStart": "11:15AM",
                "timeEnd": "12:05PM",
                "instructors": [],
                "pattern": "F"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          },
          {
            "ssrComponent": "DIS",
            "section": "202",
            "classNbr": 5670,
            "meetings": [
              {
                "timeStart": "03:35PM",
                "timeEnd": "04:25PM",
                "instructors": [],
                "pattern": "F"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          }
        ],
        "unitsMaximum": 4
      }
    ],
    "description": "This course explores both syntax (how words and phrases are 
    combined into sentences) and semantics (how the meanings of words, phrases, 
    and sentences are interpreted)."
  }
]
|}

let ling_coursedata = Coursedata.from_json ling_json
let ling_course = List.hd ling_coursedata

let make_ling_test name f = 
  "Coursedata_test.ling: " ^ name >:: f

let ling_tests = [
  make_ling_test "lectures are correct" begin fun _ -> 
    let ling_lectures = 
      [{
        shorthand = "LEC 001"; 
        kind = Lec;
        course_number = 5668;
        meeting_times = [(Thu, 610, 685); (Tue, 610, 685)];
        course_shorthand = "LING 3303"
      }] in 
    assert_equal ling_lectures ling_course.lectures 
  end;

  make_ling_test "discussions are correct" begin fun _ -> 
    let discussions = 
      [{
        shorthand = "DIS 201"; 
        kind = Dis;
        course_number = 5669;
        meeting_times = [(Fri, 675, 725)];
        course_shorthand = "LING 3303"
      };
       {
         shorthand = "DIS 202";
         kind = Dis;
         course_number = 5670;
         meeting_times = [(Fri, 935, 985)];
         course_shorthand = "LING 3303"
       }] in 
    assert_equal discussions ling_course.discussions
  end;

  make_ling_test "labs are correct" begin fun _ -> 
    let labs = 
      [] in 
    assert_equal labs ling_course.labs 
  end;

  make_ling_test "metadata is correct" begin fun _ -> 
    assert_equal "Intro to Syntax & Semantics" ling_course.name;
    assert_equal "This course explores both syntax (how words and phrases are 
    combined into sentences) and semantics (how the meanings of words, phrases, 
    and sentences are interpreted)."
      ling_course.description;
    assert_equal "LING 3303" ling_course.shorthand;
  end;

  make_minimal_test "credits is correct" begin fun _ -> 
    assert_equal 4 ling_course.credits 
  end
]

let algo_json = Yojson.Basic.from_string {|
[
  {
    "subject": "CS",
    "catalogNbr": "4820",
    "titleShort": "Intro Analysis of Algorithms",
    "enrollGroups": [
      {
        "classSections": [
          {
            "ssrComponent": "LEC",
            "section": "001",
            "classNbr": 12394,
            "meetings": [
              {
                "timeStart": "09:05AM",
                "timeEnd": "09:55AM",
                "instructors": [
                  {}
                ],
                "pattern": "MWF"
              }
            ],
            "locationDescr": "Ithaca, NY (Main Campus)"
          }
        ],
        "unitsMaximum": 4
      }
    ],
    "description": "Develops techniques used in the design and analysis of 
    algorithms, with an emphasis on problems arising in computing applications. 
    Example applications are drawn from systems and networks, 
    artificial intelligence, computer vision, data mining, and computational 
    biology. This course covers four major algorithm design techniques."
  }
]
|}

let algo_coursedata = Coursedata.from_json algo_json
let algo_course = List.hd algo_coursedata

let make_algo_test name f = 
  "Coursedata_test.algo: " ^ name >:: f

let algo_tests = [
  make_algo_test "lectures are correct" begin fun _ -> 
    let lectures = 
      [{
        shorthand = "LEC 001"; 
        kind = Lec;
        course_number = 12394;
        meeting_times = [(Fri, 545, 595); (Wed, 545, 595); (Mon, 545, 595)];
        course_shorthand = "CS 4820"
      }] in 
    assert_equal lectures algo_course.lectures 
  end;

  make_algo_test "discussions are correct" begin fun _ -> 
    let discussions = 
      [] in 
    assert_equal discussions algo_course.discussions
  end;

  make_ling_test "labs are correct" begin fun _ -> 
    let labs = 
      [] in 
    assert_equal labs algo_course.labs 
  end;

  make_ling_test "metadata is correct" begin fun _ -> 
    assert_equal "Intro Analysis of Algorithms" algo_course.name;
    assert_equal "Develops techniques used in the design and analysis of 
    algorithms, with an emphasis on problems arising in computing applications. 
    Example applications are drawn from systems and networks, 
    artificial intelligence, computer vision, data mining, and computational 
    biology. This course covers four major algorithm design techniques."
      algo_course.description;
    assert_equal "CS 4820" algo_course.shorthand;
  end;

  make_minimal_test "credits is correct" begin fun _ -> 
    assert_equal 4 algo_course.credits 
  end
]

let tests = List.flatten [minimal_tests; ling_tests; algo_tests]