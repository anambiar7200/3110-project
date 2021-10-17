open OUnit2
open Game
open Game.Card
open Game.Table

let card0 = List.nth card_deck 0

let card1 = List.nth card_deck 1

let card2 = List.nth card_deck 2

let card3 = List.nth card_deck 3

let card103 = List.nth card_deck 103

let card30 = List.nth card_deck 30

let black_7 = List.nth card_deck 19

let blue_7_2 = List.nth card_deck 45

let red_7 = List.nth card_deck 84

let orange_7 = List.nth card_deck 58

let blue_2 = List.nth card_deck 27

let orange_3 = List.nth card_deck 54

let blue_7 = List.nth card_deck 32

let blue_10 = List.nth card_deck 35

let set1_4 = create_set Run [ card0; card1; card2; card3 ]

let set4_1 = create_set Run [ card3; card1; card2; card3 ]

let set_empty = create_set Run []

let sevens3 = create_set Group [ black_7; blue_7; red_7 ]

let sevens4 = create_set Group [ black_7; blue_7; orange_7; red_7 ]

let sevens_same = create_set Group [ black_7; blue_7; blue_7_2 ]

let black1_blue2_orange3 = create_set Run [ card0; blue_2; orange_3 ]

let non_consec_blues = create_set Run [ blue_2; blue_7; blue_10 ]

let get_number_test (name : string) (c : card) (expected_output : int) :
    test =
  name >:: fun _ -> assert_equal expected_output (get_number c)

let get_color_test
    (name : string)
    (c : card)
    (expected_output : color_type) : test =
  name >:: fun _ -> assert_equal expected_output (get_color c)

let get_index_test (name : string) (c : card) (expected_output : int) :
    test =
  name >:: fun _ -> assert_equal expected_output (get_index c)

let valid_set_test
    (name : string)
    (cards : set)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (valid_set cards)

let card_tests =
  [
    get_number_test "card0 number is 1" card0 1;
    get_number_test "card103 number is 13" card103 13;
    get_number_test "card30 number is 5" card30 5;
    get_color_test "card0 color is Black" card0 Black;
    get_color_test "card103 color is Red" card103 Red;
    get_color_test "card30 color is Blue" card30 Blue;
    get_index_test "card0 index is 0" card0 0;
    get_index_test "card103 index is 103" card103 103;
    get_index_test "card30 index is 30" card30 30;
  ]

let table_tests =
  [
    valid_set_test "Empty set" set_empty false;
    valid_set_test "Black 1-4" set1_4 true;
    valid_set_test "Black 4-1" set4_1 true;
    valid_set_test "4 sevens" sevens4 true;
    valid_set_test "3 sevens" sevens3 true;
    valid_set_test "diff color run" black1_blue2_orange3 false;
    valid_set_test "rep color group" sevens_same false;
    valid_set_test "non consec run" non_consec_blues false;
  ]

let suite = "test suite for A2" >::: List.flatten [ card_tests ]

let _ = run_test_tt_main suite