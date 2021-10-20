open OUnit2
open Game
open Game.Card
open Game.Player
open Game.Drawing
open Game.Table
open Game.State

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

(* -------- player hands and moves for module player----------- *)
let p1 =
  [ card0; card100; card103; card99; card12; card36; card32; card85 ]

let p1_disordered =
  [ card103; card99; card36; card0; card100; card12; card32; card85 ]

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

let player1_disordered = build_player p1_disordered

let player2 = build_player p2

let player3 = build_player p3

let card0_player = build_player [ card0 ]

let card12_player = build_player [ card12 ]

(**Players' Hand after playing a card

   - If Player tries to play a card they do not own, raise exception
     NotYourCard*)
let play1 = build_player p11
(*card 99 out*)

(**card 85 out*)
let play2 = build_player p12

let play3 = build_player p13 (*card 0 out*)

let play4 = build_player p14
(*card 100 out*)

(**Players' Hand after taking a card back

   - If player tries to take back a card did not belong to them, raise
     exception NotYourCard*)
let pl_bk1 = play1

let pl_bk11 = build_player p1_b

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

(* -------------- states for module state----------------- *)
let init_st = State.init_state

let init_deck = State.current_deck_lst init_st

let current_player_hand = State.current_player_hand init_st

let current_table_lst = State.current_table_lst init_st

let pl_command1 =
  Command.Play
    [
      "run"; "10"; "blue"; "35"; "11"; "blue"; "36"; "12"; "blue"; "37";
    ]

let draw_command1 = Command.Draw

let stop_command1 = Command.Stop

let go_test
    (name : string)
    (cmd : Command.command)
    (st : State.state)
    (expected_output : State.result) : test =
  name >:: fun _ -> assert_equal expected_output (go cmd st)

let match_result (r : result) =
  match r with
  | Illegal -> State.init_state
  | LegalStop -> State.init_state
  | Legal st -> st

let go_draw_test
    (name : string)
    (cmd : Command.command)
    (st : State.state)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (State.current_player_hand (match_result (go cmd st))
    > State.current_player_hand st)

let state_tests =
  [
    go_test "user command stop" stop_command1 init_st State.LegalStop;
    go_test "user command illegal group"
      (Command.Play
         [
           "group";
           "10";
           "blue";
           "35";
           "11";
           "blue";
           "36";
           "12";
           "blue";
           "37";
         ])
      init_st Illegal;
    go_test "user command is legal but player does not have those cards"
      pl_command1 init_st Illegal;
    go_draw_test "user draw command" draw_command1 init_st true;
  ]

(*card not from the table*)
(* let take2 = take_from_table card12 p2 player1 *)
(*card not belong to original set*)
(* let take3 = take_from_table card70 p2 player3 *)
let deck_alot =
  [
    card0;
    card2;
    card3;
    card103;
    card30;
    card19;
    card45;
    card84;
    card50;
    card27;
    card54;
    card32;
    card35;
    card1;
  ]

let deck_one = [ card0 ]

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

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
  name >:: fun _ ->
  assert_equal expected_output (play_card c p) ~cmp:player_compare

let play_card2_test
    (name : string)
    (id : int)
    (p : player)
    (expected_output : player) : test =
  name >:: fun _ ->
  assert_equal expected_output (play_card2 id p) ~cmp:player_compare

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
  name >:: fun _ ->
  assert_equal expected_output (card_back c p bf) ~cmp:player_compare

let take_from_table_test
    (name : string)
    (c : card)
    (tb : card list)
    (bf : player)
    (expected_output : card list) : test =
  name >:: fun _ ->
  assert_equal expected_output (take_from_table c tb bf)

let add_to_player_test
    (name : string)
    (p : player)
    (c : card)
    (expected_output : player) : test =
  name >:: fun _ ->
  assert_equal expected_output (add_to_player p c)
    ~cmp:Player.player_compare

let player_compare_test
    (name : string)
    (p1 : player)
    (p2 : player)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (player_compare p1 p2)

(**-------test functions for module drawing---------*)
let right_number num = if num >= 1 && num <= 14 then true else false

let deal_test (name : string) =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists deck_alot
    (fst (Drawing.deal deck_alot))

let deal_return_test (name : string) =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists [] (snd (Drawing.deal deck_alot))

let deal_error_test (name : string) =
  name >:: fun _ ->
  assert_raises OutOfCards (fun () -> Drawing.deal deck_one)

let draw_test (name : string) =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists deck_one
    [ fst (Drawing.draw deck_one) ]

let draw_return_test (name : string) =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists [] (snd (Drawing.draw deck_one))

let draw_error_test (name : string) =
  name >:: fun _ -> assert_raises OutOfCards (fun () -> Drawing.draw [])

let drawing_init_test (name : string) =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists Card.card_deck
    (Drawing.drawing_init ())

let play_card_exception_test (name : string) (c : card) (p : player) :
    test =
  name >:: fun _ ->
  assert_raises Game.Player.OutOfCards (fun () -> play_card c p)

let card_back_exception_test
    (name : string)
    (c : card)
    (p : player)
    (bf : player) : test =
  name >:: fun _ ->
  assert_raises Game.Player.NotYourCard (fun () -> card_back c p bf)

let take_from_table_nyc_exception_test
    (name : string)
    (c : card)
    (tb : Card.card list)
    (bf : player) : test =
  name >:: fun _ ->
  assert_raises Game.Player.NotYourCard (fun () ->
      take_from_table c tb bf)

let take_from_table_nsc_exception_test
    (name : string)
    (c : card)
    (tb : Card.card list)
    (bf : player) : test =
  name >:: fun _ ->
  assert_raises Game.Table.NoSuchCard (fun () ->
      take_from_table c tb bf)

(**-------test functions for module state ---------*)
let go_test
    (name : string)
    (cmd : Command.command)
    (st : State.state)
    (expected_output : State.result) : test =
  name >:: fun _ -> assert_equal expected_output (go cmd st)

let match_result (r : result) =
  match r with
  | Illegal -> State.init_state
  | Legal st -> st

let go_draw_test
    (name : string)
    (cmd : Command.command)
    (st : State.state)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (State.current_player_hand (match_result (go cmd st))
    > State.current_player_hand st)

(**-------test suites---------*)
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
    play_card2_test "1 player 1 plays card99" 99 player1 play1;
    play_card2_test "2 player 1 then plays card85" 85 play1 play2;
    play_card2_test "3 player 1 then plays card0" 0 play2 play3;
    play_card2_test "4 player 1 then plays card100" 100 play3 play4;
    card_back_test "taking card99 back to player1" card99 pl_bk1 player1
      pl_bk11;
    insert_to_table_test "insert card70 to p1 at index 4" card70 p1 4 0
      pl_tb1;
    insert_to_table_test "insert card103 to p3 at index 0" card103 p3 0
      0 pl_tb2;
    take_from_table_test "take card30 from p2 back to player3" card30 p2
      player3 take1;
    play_card_exception_test "player1 tries to play card70" card70
      player1;
    card_back_exception_test "play2 tries to take card70 back" card70
      play2 player1;
    card_back_exception_test "player2 tries to take card100 back"
      card100 player2 player2;
    take_from_table_nsc_exception_test
      "player1 tires to take card12 from p2" card12 p2 player1;
    take_from_table_nyc_exception_test
      "player3 tires to take card70 from p2" card70 p2 player3;
    player_compare_test "p1 w/ diff order" player1 player1_disordered
      true;
    player_compare_test "player 1 and player 2 are different" player1
      player2 false;
    player_compare_test "card0 Black 1 and card12 Black 1" card0_player
      card12_player false;
    player_compare_test "empty and empty" empty empty true;
    add_to_player_test "play4 draws card100" play4 card100 play3;
    add_to_player_test "play3 draws card0" play3 card0 play2;
    add_to_player_test "play2 draws card85" play2 card85 play1;
    add_to_player_test "play1 draws card99" play1 card99 player1;
  ]

let drawing_tests =
  [
    deal_test "14 cards";
    deal_return_test "14 cards, empty remaining deck";
    draw_test "1 card";
    draw_return_test "1 card,  empty remaining deck";
    deal_error_test
      "draw 14 cards from a deck of 1, raise OutOfCards error";
    draw_error_test
      "draw 1 card from a empty deck, raise OutOfCards error";
    drawing_init_test "test init and shuffle";
  ]

let state_tests =
  [
    go_test "user command stop" stop_command1 init_st (Legal init_st);
    go_test "user command illegal group"
      (Command.Play
         [
           "group";
           "10";
           "blue";
           "35";
           "11";
           "blue";
           "36";
           "12";
           "blue";
           "37";
         ])
      init_st Illegal;
    go_test "user command is legal but player does not have those cards"
      pl_command1 init_st Illegal;
    go_draw_test "user draw command" draw_command1 init_st true;
  ]

let suite =
  "test suite for A2"
  >::: List.flatten
         [
           card_tests;
           table_tests;
           player_tests;
           drawing_tests;
           state_tests;
         ]

let _ = run_test_tt_main suite