open Game
open Card
open Table
open Player
open Drawing
open State
open Command
open Graph
open Graphics
open Add

let valid_command_form =
  "Please enter a case sensitive valid command with the first word \
   being draw, stop or play followed by run or group and list of \
   cards. cards element in a list of cards must be valid."

let empty_command_message = "You have entered an empty command. "

let malformed_command_message = "This is a malformed command. "

let illegal_move_message = "This move is illegal. "

let illegal_first_play_message =
  "Your first play must have cards' values add up to at least 20"

let exceed_limit_message = "You can't draw more cards (limit: 60)"

let farewell_message = "Thank you for playing this game. Bye!"

let endturn_message =
  "The other player has ended their turn. " ^ valid_command_form

let get_color_str color_in =
  match color_in with
  | Black -> "black"
  | Blue -> "blue"
  | Orange -> "orange"
  | Red -> "red"
  | Joker -> "joker"

let print_card c =
  let number_str = string_of_int (get_number c) in
  let color_str = get_color_str (get_color c) in
  let index_str = string_of_int (get_index c) in
  "{ number: " ^ number_str ^ ", color: " ^ color_str ^ ", index: "
  ^ index_str ^ " }"

let rec print_list lst =
  match lst with
  | [] -> ()
  | h :: t ->
      print_endline (print_card h);
      print_list t

let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(** get new set added input str1 = "group" or "run str2 = "1 black 0 1
    blue 26 1 orange 52" ouput set of str2" *)
let get_added_set (st : state) ((str1, str2) : string * string list) =
  let new_state = play_state st (str1, str2) in
  let current_table = current_table_lst new_state in
  let table_row = List.length current_table in
  let last_row = List.nth current_table (table_row - 1) in
  let row_len = List.length last_row in
  let new_set = List.nth last_row (row_len - 1) in
  new_set

let white_deck_size_message () =
  set_color Graphics.white;
  fill_rect 1055 5 145 60;
  set_color Graphics.black

let display_deck_size (st : state) =
  let remain_card =
    106
    - player_size (current_player_hand st)
    - player_size (current_next_player st)
  in
  moveto 1055 20;
  draw_string ("Deck Size: " ^ string_of_int remain_card)

let white_turn () =
  set_color Graphics.white;
  fill_rect 500 5 100 20;
  set_color Graphics.black

let display_turn (st : state) =
  moveto 500 5;
  let current = current_count st in
  match current mod 2 with
  | 0 -> draw_string "Player 1's turn"
  | _ -> draw_string "Player 2's turn"

let white_error_message () =
  set_color Graphics.white;
  fill_rect 300 570 500 30;
  set_color Graphics.black

let display_message (str : string) =
  moveto 300 570;
  set_color Graphics.red;
  draw_string str;
  set_color Graphics.black

let white_player_hand () =
  moveto 150 65;
  set_color Graphics.white;
  fill_rect 150 5 900 95;
  set_color Graphics.black

let white_next_player_hand () =
  set_color Graphics.white;
  fill_rect 0 35 150 565;
  set_color Graphics.black

let redraw_table state new_set =
  let added_set = state |> current_table_lst |> add_set new_set in
  let size = new_set |> set_size in
  draw_set (draw_index added_set) size size new_set

let clear_graph state new_set =
  Graphics.clear_graph ();
  let added_set = state |> current_table_lst |> add_set new_set in
  draw_current_player (current_player_hand state) 150 65;
  let size = new_set |> set_size in
  draw_set (draw_index added_set) size size new_set;
  draw_rect 0 0 35 30;
  moveto 5 10;
  set_color (rgb 153 0 0);
  draw_string "STOP";
  set_color black;
  draw_rect 40 0 35 30;
  moveto 45 10;
  set_color (rgb 51 0 102);
  draw_string "DRAW";
  set_color black;
  draw_rect 80 0 55 30;
  moveto 85 10;
  set_color (rgb 102 102 0);
  draw_string "ENDTURN";
  set_color black;
  draw_next_player (current_next_player init_state) 15 550;
  display_turn state

let rec match_command state command_string command =
  let result = go command state in
  match result with
  | Legal new_state ->
      let string_list = String.split_on_char ' ' command_string in
      if List.hd string_list = "play" then (
        white_error_message ();
        white_turn ();
        white_player_hand ();
        display_turn new_state;
        draw_current_player (current_player_hand new_state) 135 65;
        let rest = List.tl string_list in
        (let new_set =
           get_added_set state (List.hd rest, List.tl rest)
         in
         redraw_table new_state new_set);
        ask_for_command new_state)
      else if List.hd string_list = "draw" then (
        white_error_message ();
        white_turn ();
        white_deck_size_message ();
        white_player_hand ();
        white_next_player_hand ();
        display_turn new_state;
        draw_current_player (current_player_hand new_state) 135 65;
        draw_next_player (current_next_player new_state) 15 550;
        display_deck_size new_state;
        ask_for_command new_state)
      else ask_for_command new_state
  | LegalSwitch st ->
      white_error_message ();
      white_turn ();
      white_player_hand ();
      white_next_player_hand ();
      draw_current_player (current_player_hand st) 135 65;
      draw_next_player (current_next_player st) 15 550;
      display_turn st;
      ask_for_command st
  | Illegal ->
      white_error_message ();
      display_message illegal_move_message;
      print_endline illegal_move_message;
      ask_for_command state
  | LegalStop ->
      white_error_message ();
      display_message farewell_message;
      print_endline farewell_message;
      exit 0
  | IllegalLimit ->
      white_error_message ();
      display_message exceed_limit_message;
      print_endline exceed_limit_message;
      ask_for_command state
  | IllegalFirstPlay ->
      white_error_message ();
      display_message illegal_first_play_message;
      print_endline illegal_first_play_message;
      ask_for_command state

and ask_for_command state =
  print_list (current_player_hand state);
  print_endline valid_command_form;
  let command_string = read_line () in
  try
    parse_input command_string |> match_command state command_string
  with
  | Empty ->
      print_endline empty_command_message;
      ask_for_command state
  | Malformed ->
      print_endline malformed_command_message;
      ask_for_command state

let play_game () =
  init_window 2;
  init_state |> ask_for_command

let rec game (st : state) () =
  let event = wait_next_event [ Button_down; Button_up ] in
  let position = (event.mouse_x, event.mouse_y) in
  match position with
  | x, y ->
      if x >= 0 && x <= 60 && y >= 0 && y <= 30 then
        State.go Command.Stop st
      else if x >= 65 && x <= 125 && y >= 0 && y <= 30 then
        State.go Command.Draw st
      else if x >= 130 && x <= 190 && y >= 0 && y <= 30 then
        State.go Command.EndTurn st
      else Legal st

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n\
     Welcome to MS1 for the Rummikub Game Engine (CS 3110 Final \
     Project).\n";
  print_endline
    "Authors: Ja Young Byun (jb2297), Yuyi He (yh383), Danyu Hu \
     (dh573), Anusha Nambiar (aan29)";
  print_endline "Hit enter to start Rummikub and view our demo: \n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | _ -> play_game ()

let () = main ()
