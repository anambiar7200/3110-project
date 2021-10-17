open OUnit2
open Game
open Game.Card
open Game.Player
open Game.Drawing
open Game.Table

(**---------------------------cards for testing-------------------------*)

(*Black 1*)
let card0 = List.nth card_deck 0

(*Black 3*)
let card2 = List.nth card_deck 2

(*Black 4*)
let card3 = List.nth card_deck 3

(*Red 13*)
let card103 = List.nth card_deck 103

(*Blue 5*)
let card30 = List.nth card_deck 30

(*Black 7*)
let card19 = List.nth card_deck 19

(*Blue 7 (2)*)
let card45 = List.nth card_deck 45

(*Red 7*)
let card84 = List.nth card_deck 84

(*Orange 7*)
let card58 = List.nth card_deck 58

(*Blue 2*)
let card27 = List.nth card_deck 27

(*Orange 3*)
let card54 = List.nth card_deck 54

(*Blue 7 (1)*)
let card32 = List.nth card_deck 32

(*Blue 10*)
let card35 = List.nth card_deck 35

(*Black 2*)
let card1 = List.nth card_deck 1

(*Blue 12*)
let card50 = List.nth card_deck 50

(*Orange 6*)
let card70 = List.nth card_deck 70

(*Orange 2*)
let card66 = List.nth card_deck 66

(*Orange 5*)
let card69 = List.nth card_deck 69

(*Red 10*)
let card100 = List.nth card_deck 100

(*Orange 1*)
let card52 = List.nth card_deck 52

(*Blue 7*)
let card32 = List.nth card_deck 32

(*Blue 11*)
let card36 = List.nth card_deck 36

(*Red 9*)
let card99 = List.nth card_deck 99

(*Red 8*)
let card85 = List.nth card_deck 85

(*Black 13*)
let card12 = List.nth card_deck 12

(**----------------- sets and tables for testing----------------*)
let black_1_4 = create_set Run [ card0; card1; card2; card3 ]

(*let black_4_1 = create_set Run [ card3; card2; card1; card0 ] *)

let set_empty = create_set Run []

let sevens3 = create_set Group [ card58; card32; card84 ]

let sevens4 = create_set Group [ card19; card32; card58; card84 ]

let sevens_same = create_set Group [ card19; card32; card45 ]

let black1_blue2_orange3 = create_set Run [ card0; card27; card54 ]

let non_consec_blues = create_set Run [ card27; card45; card35 ]

let invalid_table_ii = create_table [ non_consec_blues; sevens_same ]

let invalid_table_iv = create_table [ non_consec_blues; sevens4 ]

let valid_table = create_table [ black_1_4; sevens3 ]

let empty_table = create_table []

(* ------------ player hands and moves for module player
   ------------- *)
let p1 =
  [ card0; card100; card103; card99; card12; card36; card32; card85 ]

let p1_b =
  [ card99; card0; card100; card103; card12; card36; card32; card85 ]

let p2 = [ card103; card70; card30 ]

let p21 =
  [
    card0;
    card100;
    card103;
    card99;
    card70;
    card12;
    card36;
    card32;
    card85;
  ]

let p22 = [ card103; card70 ]

let p3 = [ card1; card66; card69; card30 ]

let p31 = [ card103; card1; card66; card69; card30 ]

let p11 = [ card0; card100; card103; card12; card36; card32; card85 ]

let p12 = [ card0; card100; card103; card12; card36; card32 ]

let p13 = [ card100; card103; card12; card36; card32 ]

let p14 = [ card103; card12; card36; card32 ]

(**Players' Hands*)
let player1 = build_player p1

let player2 = build_player p2

let player3 = build_player p3

(**Players' Hand after playing a card

   - If Player tries to play a card they do not own, raise exception
     NotYourCard*)
let play1 = build_player p11
(*card 99*)

(**card 85*)
let play2 = build_player p12

let play3 = build_player p13 (*card 0*)

let play4 = build_player p14
(*card 100*)

(* let play5 = play_card card70 player1 *)

(**Players' Hand after taking a card back

   - If player tries to take back a card did not belong to them, raise
     exception NotYourCard*)
let pl_bk1 = play1

let pl_bk11 = build_player p1_b

(* let pl_bk2 = card_back card70 play2

   let pl_bk3 = card_back card100 player2 *)

(*Players' Hand after drawing a card from the deck - If the deck is
  empty, raise exception OutOfCards*)
(* let pdraw1 = draw_to_player player1

   let pdraw2 = draw_to_player player2

   let pdraw3 = draw_to_player player3 *)

(**A set on the table after the player tries to play a card*)

let pl_tb1 = p21
(*insert_to_table card70 p1 4 0*)

let pl_tb2 = p31
(*insert_to_table card103 p3 0 0*)

(**A set on the table after the player tries to take a card

   - if the card was not originally in the player's hand, raise
     exception NotYourCard
   - if the card is not in the table list, raise exception NoSuchCard*)

(*no error*)
let take1 = p22
(*take_from_table card30 p2 player3*)

(*card not from the table*)
(* let take2 = take_from_table card12 p2 player1 *)
(*card not belong to original set*)
(* let take3 = take_from_table card70 p2 player3 *)

(**------------test functions for module cards-------------*)

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

(**-------------test functions for module table-------------------*)
let valid_set_test
    (name : string)
    (cards : set)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (valid_set cards)

let valid_table_test
    (name : string)
    (t : table)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (Game.Table.valid_table t)

(**-------test functions for module player---------*)

let build_peek_player_test
    (name : string)
    (clst : card list)
    (expected_output : card) : test =
  name >:: fun _ ->
  assert_equal expected_output (peek_player (build_player clst))

let play_card_test
    (name : string)
    (c : card)
    (p : player)
    (expected_output : player) : test =
  name >:: fun _ -> assert_equal expected_output (play_card c p)

let insert_to_table_test
    (name : string)
    (c : card)
    (tb : card list)
    (ind : int)
    (cur : int)
    (expected_output : card list) : test =
  name >:: fun _ ->
  assert_equal expected_output (insert_to_table c tb ind cur)

let card_back_test
    (name : string)
    (c : card)
    (p : player)
    (bf : player)
    (expected_output : player) : test =
  name >:: fun _ -> assert_equal expected_output (card_back c p bf)

let take_from_table_test
    (name : string)
    (c : card)
    (tb : card list)
    (bf : player)
    (expected_output : card list) : test =
  name >:: fun _ ->
  assert_equal expected_output (take_from_table c tb bf)

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
    valid_set_test "Black 1-4" black_1_4 true;
    (* valid_set_test "Black 4-1" black_4_1 true; *)
    valid_set_test "4 sevens" sevens4 true;
    valid_set_test "3 sevens" sevens3 true;
    valid_set_test "diff color run" black1_blue2_orange3 false;
    valid_set_test "rep color group" sevens_same false;
    valid_set_test "non consec run" non_consec_blues false;
    valid_table_test "empty table" empty_table true;
    valid_table_test "sets: valid, invalid" invalid_table_iv false;
    valid_table_test "sets: invalid, invalid" invalid_table_ii false;
    valid_table_test "valid table" valid_table true;
  ]

let player_tests =
  [
    build_peek_player_test "build player 1" p1 card0;
    build_peek_player_test "build player 2" p2 card103;
    build_peek_player_test "build player 3" p3 card1;
    play_card_test "1player 1 plays card99" card99 player1 play1;
    play_card_test "2player 1 then plays card85" card85 play1 play2;
    play_card_test "3player 1 then plays card0" card0 play2 play3;
    play_card_test "4player 1 then plays card100" card100 play3 play4;
    card_back_test "taking card99 back to player1" card99 pl_bk1 player1
      pl_bk11;
    insert_to_table_test "insert card70 to p1 at index 4" card70 p1 4 0
      pl_tb1;
    insert_to_table_test "insert card103 to p3 at index 0" card103 p3 0
      0 pl_tb2;
    take_from_table_test "take card30 from p2 back to player3" card30 p2
      player3 take1;
  ]

let suite =
  "test suite for A2"
  >::: List.flatten [ card_tests; table_tests; player_tests ]

let _ = run_test_tt_main suite