open OUnit2
open Game
open Game.Card
open Game.Player
open Game.Drawing
open Game.Table
open Game.State
open Game.Add
open Game.Command

(**---------------------------cards for testing-------------------------*)

(*Black 1*)
let card0 = List.nth card_deck 0

(*Black 3*)
let card2 = List.nth card_deck 2

(*Black 4*)
let card3 = List.nth card_deck 3

(* Red 12*)
let card102 = List.nth card_deck 102

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

(*Blue 1*)
let card26 = List.nth card_deck 26

(*Red 1*)
let card78 = List.nth card_deck 78

let joker1 = List.nth card_deck2 104

let joker2 = List.nth card_deck2 105

let card10 = List.nth card_deck2 10

let card4 = List.nth card_deck2 4

(**----------------- sets and tables for testing----------------*)
let black_1_4 = create_set Run [ card0; card1; card2; card3 ]

(*let black_4_1 = create_set Run [ card3; card2; card1; card0 ] *)
let black1_blue2_joker = create_set Run [ card0; card27; joker1 ]

let black_1_2_joker = create_set Run [ card0; card1; joker2 ]

let black_1_4_joker =
  create_set Run [ card0; card1; card2; card3; joker2 ]

let black_1_2_joker_3 = create_set Run [ card0; card1; joker1; card3 ]

let joker_joker_black1 = create_set Run [ joker1; joker2; card0 ]

let joker_black_1_4 =
  create_set Run [ joker2; card0; card1; card2; card3 ]

let red_12_13_joker = create_set Run [ card102; card103; joker2 ]

let set_empty = create_set Run []

let sevens3 = create_set Group [ card58; card32; card84 ]

let sevens3_joker = create_set Group [ card58; card32; joker1; card84 ]

let sevens2_joker = create_set Group [ card32; card84; joker2 ]

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

let p22 = [ card103; card78 ]

let p3 = [ card1; card66; card69; card30 ]

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

let play2 = build_player p12
(*card 85 out*)

let play3 = build_player p13
(*card 0 out*)

let play4 = build_player p14
(*card 100 out*)

(**Players' Hand after taking a card back

   - If player tries to take back a card did not belong to them, raise
     exception NotYourCard*)
let pl_bk1 = play1

let pl_bk11 = build_player p1_b

(**Test insert_to_set and take_from_set*)
let player_st = build_player p22
(*player originally has: card103; card78*)

let st_group1 = create_set Group [ card0; card26; card52 ]
(*original set*)

let st_group2 = create_set Group [ card78; card0; card26; card52 ]
(*insert to the front*)

let st_group3 = create_set Group [ card0; card26; card52; card78 ]
(*insert to the back*)

let st_group4 = create_set Group [ card0; card26; card78; card52 ]
(*insert to the middle*)

(* -------------- states for module state----------------- *)
let init_st = init_state
(*initial state*)

let init_deck = current_deck_lst init_st
(*current deck of the initial state*)

let current_player_hand = current_player_hand init_st
(*current player hand of the initial state*)

let current_table_lst = current_table_lst init_st
(*current table of the initial state*)

let next_pl = current_next_player init_st

let test_state =
  create_state init_deck current_table_lst next_pl current_player_hand
    [ 0; 0 ] 1

let m_pl1 = [ card10; joker1; joker2; card4; card19 ]

let m_pl2 = [ card30; card103; card102; card26 ]

let edit_table1 =
  [
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
  ]

let edit_b = create_state card_deck2 edit_table1 m_pl1 m_pl2 [ 1; 1 ] 2

(*command to play a valid run*)
let pl_command1 =
  Play
    [
      "run"; "10"; "blue"; "35"; "11"; "blue"; "36"; "12"; "blue"; "37";
    ]

(*command to draw*)
let draw_command1 = Draw

(*command to stop*)
let stop_command1 = Stop

(*invalid edit*)
let edit_command1 = Edit [ "pre"; "1"; "1"; "10" ]

let edit_command2 = Edit [ "post"; "1"; "1"; "10" ]

let edit_command5 = Edit [ "post"; "0"; "3"; "10" ]

let edit_command3 = Edit [ "pre"; "2"; "2"; "10" ]

(*valid_edit*)
let edit_command4 = Edit [ "post"; "0"; "2"; "4" ]

let black_1_4p = create_set Run [ card0; card1; card2; card3; card4 ]

let edit_table4 =
  [
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4p ];
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
  ]

let m_pl14 = [ card10; joker1; joker2; card19 ]

let edit_b4 =
  create_state card_deck2 edit_table4 m_pl14 m_pl2 [ 2; 1 ] 2

let edit_command9 = Edit [ "post"; "2"; "2"; "4" ]

let edit_table9 =
  [
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4p ];
  ]

let edit_b9 =
  create_state card_deck2 edit_table9 m_pl14 m_pl2 [ 2; 1 ] 2

let edit_command10 = Edit [ "pre"; "2"; "2"; "104" ]

let edit_command11 = Edit [ "post"; "2"; "2"; "105" ]

let black_1_4pj1 = create_set Run [ joker1; card0; card1; card2; card3 ]

let black_1_4pj2 = create_set Run [ card0; card1; card2; card3; joker2 ]

let m_pl110 = [ card10; joker2; card4; card19 ]

let m_pl111 = [ card10; joker1; card4; card19 ]

let edit_table10 =
  [
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4pj1 ];
  ]

let edit_table11 =
  [
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4pj2 ];
  ]

let edit_b10 =
  create_state card_deck2 edit_table10 m_pl110 m_pl2 [ 2; 1 ] 2

let edit_b11 =
  create_state card_deck2 edit_table11 m_pl111 m_pl2 [ 2; 1 ] 2

let sevens3p = create_set Group [ card19; card58; card32; card84 ]

let sevens3a = create_set Group [ card58; card32; card84; card19 ]

let sevens3pj1 = create_set Group [ joker1; card58; card32; card84 ]

let sevens3aj2 = create_set Group [ card58; card32; card84; joker2 ]

let edit_command6 = Edit [ "pre"; "1"; "1"; "104" ]

let edit_command7 = Edit [ "post"; "1"; "1"; "105" ]

let edit_table6 =
  [
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
    [ black_1_4; empty_set; sevens3pj1; empty_set; black_1_4 ];
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
  ]

let edit_table7 =
  [
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
    [ black_1_4; empty_set; sevens3aj2; empty_set; black_1_4 ];
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
  ]

let edit_b6 =
  create_state card_deck2 edit_table6 m_pl110 m_pl2 [ 2; 1 ] 2

let edit_b7 =
  create_state card_deck2 edit_table7 m_pl111 m_pl2 [ 2; 1 ] 2

let edit_command8 = Edit [ "pre"; "1"; "1"; "19" ]

let edit_command66 = Edit [ "post"; "1"; "1"; "19" ]

let m_pl18 = [ card10; joker1; joker2; card4 ]

let edit_table8 =
  [
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
    [ black_1_4; empty_set; sevens3p; empty_set; black_1_4 ];
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
  ]

let edit_table66 =
  [
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
    [ black_1_4; empty_set; sevens3a; empty_set; black_1_4 ];
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
  ]

let edit_b8 =
  create_state card_deck2 edit_table8 m_pl18 m_pl2 [ 2; 1 ] 2

let edit_b66 =
  create_state card_deck2 edit_table66 m_pl18 m_pl2 [ 2; 1 ] 2

let m_pl_pl1 = build_player (get_cards black_1_4)

let new_pl_st =
  create_state card_deck2 edit_table1 m_pl_pl1 m_pl2 [ 1; 1 ] 2

let new_ppp = build_player []

let edit_table1pl =
  [
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
    [ black_1_4; empty_set; sevens3; empty_set; black_1_4 ];
    [ black_1_4 ];
  ]

let new_pl_st1 =
  create_state card_deck2 edit_table1pl m_pl2 new_ppp [ 2; 1 ] 3

let pl_command22 =
  Play
    [
      "run";
      "1";
      "black";
      "0";
      "2";
      "black";
      "1";
      "3";
      "black";
      "2";
      "4";
      "black";
      "3";
    ]

(**---------------------variable to test drawing---------------------*)

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

(**----------------string and arr for module command--------------*)
let string_space = "  play group 1 black 0 1 blue 26 1 orange 52 "

let space_arr =
  [ "group"; "1"; "black"; "0"; "1"; "blue"; "26"; "1"; "orange"; "52" ]

let string_stop = "stop"

let string_empty = "   "

let string_malformed = "play "

let string_malformed2 = "draw now"

let string_run = "play  run 10 blue 35 11 blue 36 12 blue 37"

let run_arr =
  [ "run"; "10"; "blue"; "35"; "11"; "blue"; "36"; "12"; "blue"; "37" ]

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

let play_card2_test
    (name : string)
    (id : int)
    (p : player)
    (expected_output : player) : test =
  name >:: fun _ ->
  assert_equal expected_output (play_card2 id p) ~cmp:player_compare

let insert_to_set_test
    (name : string)
    (c : card)
    (st : set)
    (ind : int)
    (cur : int)
    (expected_output : set) : test =
  name >:: fun _ ->
  assert_equal expected_output (insert_to_set c st ind cur)

let card_back_test
    (name : string)
    (c : card)
    (p : player)
    (bf : player)
    (expected_output : player) : test =
  name >:: fun _ ->
  assert_equal expected_output (card_back c p bf) ~cmp:player_compare

let take_from_set_test
    (name : string)
    (c : card)
    (st : set)
    (bf : player)
    (expected_output : set) : test =
  name >:: fun _ -> assert_equal expected_output (take_from_set c st bf)

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

let card_back_exception_test
    (name : string)
    (c : card)
    (p : player)
    (bf : player) : test =
  name >:: fun _ ->
  assert_raises Game.Player.NotYourCard (fun () -> card_back c p bf)

let take_from_set_nyc_exception_test
    (name : string)
    (c : card)
    (st : set)
    (bf : player) : test =
  name >:: fun _ ->
  assert_raises Game.Player.NotYourCard (fun () ->
      take_from_set c st bf)

let take_from_set_nsc_exception_test
    (name : string)
    (c : card)
    (st : set)
    (bf : player) : test =
  name >:: fun _ ->
  assert_raises Game.Table.NoSuchCard (fun () -> take_from_set c st bf)

(**-------test functions for module drawing---------*)
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

(**-------test functions for module command---------*)

let malformed_test (name : string) (str_input : string) : test =
  name >:: fun _ ->
  assert_raises Game.Command.Malformed (fun () ->
      Game.Command.parse_input str_input)

let empty_test (name : string) (str_input : string) : test =
  name >:: fun _ ->
  assert_raises Game.Command.Empty (fun () ->
      Game.Command.parse_input str_input)

let parse_input_test
    (name : string)
    (str_input : string)
    (expected_output : Game.Command.command) : test =
  name >:: fun _ ->
  assert_equal expected_output (Game.Command.parse_input str_input)

(**-------test functions for module state ---------*)
let go_test
    (name : string)
    (cmd : Command.command)
    (st : State.state)
    (expected_output : State.result) : test =
  name >:: fun _ -> assert_equal expected_output (go cmd st)

let match_result (r : result) =
  match r with
  | Illegal -> init_state
  | Legal st -> st
  | LegalStop -> init_state
  | LegalSwitch st -> st

let go_draw_test
    (name : string)
    (cmd : Command.command)
    (st : State.state) : test =
  name >:: fun _ ->
  assert_equal
    (go cmd st |> match_result |> State.current_next_player
   |> List.length)
    ((State.current_player_hand st |> List.length) + 1)
    ~printer:string_of_int

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
    valid_set_test "Empty set" set_empty true;
    valid_set_test "Black 1-4" black_1_4 true;
    valid_set_test "Black 1-4, joker" black_1_4_joker true;
    valid_set_test "Black 1,2, joker" black_1_2_joker true;
    valid_set_test "Black 1, Blue 2 joker" black1_blue2_joker false;
    valid_set_test "Black 1,2 joker Black 3" black_1_2_joker_3 true;
    valid_set_test "joker joker black1" joker_joker_black1 false;
    valid_set_test "joker black1_4" joker_black_1_4 false;
    valid_set_test "red12,13 joker" red_12_13_joker false;
    (* valid_set_test "Black 4-1" black_4_1 true; *)
    valid_set_test "4 sevens" sevens4 true;
    valid_set_test "3 sevens" sevens3 true;
    valid_set_test "3 sevens + joker" sevens3_joker true;
    valid_set_test "2 sevens + joker" sevens2_joker true;
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
    insert_to_set_test "insert card78 to st_group1 at index 0" card78
      st_group1 0 0 st_group2;
    insert_to_set_test "insert card78 to st_group1 at index 3" card78
      st_group1 3 0 st_group3;
    insert_to_set_test "insert card78 to st_group1 at index 2" card78
      st_group1 2 0 st_group4;
    insert_to_set_test "insert card78 to st_group1 at index 3" card78
      st_group1 3 0 st_group3;
    take_from_set_test "take card78 from st_group2 back to player_st"
      card78 st_group2 player_st st_group1;
    take_from_set_test "take card78 from st_group3 back to player_st"
      card78 st_group3 player_st st_group1;
    take_from_set_test "take card78 from st_group4 back to player_st"
      card78 st_group4 player_st st_group1;
    card_back_exception_test "play2 tries to take card70 back" card70
      play2 player1;
    card_back_exception_test "player2 tries to take card100 back"
      card100 player2 player2;
    take_from_set_nsc_exception_test
      "player_st tires to take card12 from st_group2" card12 st_group2
      player1;
    take_from_set_nyc_exception_test
      "player_st tires to take card0 from st_group2" card0 st_group2
      player_st;
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
    go_test "user command stop" stop_command1 init_st LegalStop;
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
    go_test "test switch" Command.EndTurn init_st
      (LegalSwitch test_state);
    go_draw_test "user draw command" draw_command1 init_st;
    go_test "cmd1 illegal" edit_command1 edit_b Illegal;
    go_test "cmd2 illegal" edit_command2 edit_b Illegal;
    go_test "cmd5 illegal" edit_command5 edit_b Illegal;
    go_test "cmd3 illegal" edit_command3 edit_b Illegal;
    go_test "cmd10 ilegal" edit_command10 edit_b Illegal;
    (*legal*)
    go_test "cmd8 legal" edit_command8 edit_b (Legal edit_b8);
    go_test "cmd66 legal" edit_command66 edit_b (Legal edit_b66);
    go_test "cmd6 legal" edit_command6 edit_b (Legal edit_b6);
    go_test "cmd7 legal" edit_command7 edit_b (Legal edit_b7);
    go_test "cmd11 legal" edit_command11 edit_b (Legal edit_b11);
    go_test "cmd4 legal" edit_command4 edit_b (Legal edit_b4);
    go_test "cmd9 legal" edit_command9 edit_b (Legal edit_b9);
    go_test "playyyy" pl_command22 new_pl_st (Legal new_pl_st1);
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