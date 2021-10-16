open OUnit2
open Game
open Game.Card
open Game.Player
open Game.Drawing
open Game.Table

let card0 = List.nth card_deck 0

let card103 = List.nth card_deck 103

let card30 = List.nth card_deck 30

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

let suite = "test suite for A2" >::: List.flatten [ card_tests ]

let _ = run_test_tt_main suite