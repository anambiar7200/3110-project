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

let farewell_message = "Thank you for playing this game. Bye!"

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

let rec match_command state command =
  let result = go command state in
  match result with
  | Legal new_state -> ask_for_command new_state
  | LegalSwitch st -> ask_for_command st
  | Illegal ->
      print_endline illegal_move_message;
      ask_for_command state
  | LegalStop ->
      print_endline farewell_message;
      exit 0

and ask_for_command state =
  print_list (current_player_hand state);
  print_endline valid_command_form;
  let command = read_line () in
  try parse_input command |> match_command state with
  | Empty ->
      print_endline empty_command_message;
      ask_for_command state
  | Malformed ->
      print_endline malformed_command_message;
      ask_for_command state

let clear_graph state new_set =
  Graphics.clear_graph ();
  let added_set = state |> current_table_lst |> add_set new_set in
  draw_current_player (current_player_hand state) 150 50;
  let size = new_set |> set_size in
  draw_set (draw_index added_set) size size new_set;
  draw_rect 0 0 60 30;
  moveto 13 10;
  set_color (rgb 153 0 0);
  draw_string "STOP";
  set_color black;
  draw_rect 65 0 60 30;
  moveto 78 10;
  set_color (rgb 51 0 102);
  draw_string "DRAW";
  set_color black;
  draw_rect 130 0 60 30;
  moveto 143 10;
  set_color (rgb 102 102 0);
  draw_string "ENDTURN";
  set_color black;
  draw_next_player (current_next_player init_state) 50 540

let is_changed (card : card) (already_clicked : card list) =
  let length = List.length already_clicked in
  let new_clicked =
    List.filter (fun x -> get_index x <> get_index card) already_clicked
  in
  let new_length = List.length new_clicked in
  (length <> new_length, new_clicked)

let color_select_card
    (cd : card)
    (x : int)
    (y : int)
    (already_clicked : card list) =
  moveto x y;
  draw_rect x y 30 30;
  let is_changed cd already_clicked then set_color Graphics.red
  else set_color Graphics.yellow;
  fill_rect x y 30 30;
  moveto (x + 13) (y + 10);
  set_color (card_color cd);
  draw_string (string_of_int (get_number cd));
  set_color Graphics.black;
  if get_color cd = Joker then draw_circle (x + 15) (y + 15) 10

(** get current player with State.current_player_hand (st : state) given
    mouse position out card index in player hand. Current player cards
    are displayed at the bottom of screen. The first card left bottom
    corner is at (150, y)*)
let get_clicked_playercard
    (current_st : State.state)
    ((x, y) : int * int) =
  let player_cards = State.current_player_hand current_st in
  let float_ind = (float_of_int x -. 150.) /. 30. in
  let ind = Float.to_int (Float.floor float_ind) in
  let clicked_playercard = List.nth player_cards ind in
  clicked_playercard

let rec loop state clicked_cards =
  let e = wait_next_event [ Button_down ] in
  if e.button then
    let clicked_card =
      get_clicked_playercard state (e.mouse_x, e.mouse_y)
    in
    color_select_card clicked_card e.mouse_x e.mouse_y clicked_cards
  else loop state clicked_cards

(* if e.key <> 'q' then loop state else () *)

let play_game () =
  init_window 2;
  print_endline "let's play a game >:)";
  let _ = loop init_state [] in
  print_endline "trigger";
  init_state |> ask_for_command

(* let rec game () = let event = wait_next_event [Button_down;Button_up]
   in let position = (event.mouse_x, event.mouse_y) in match position
   with |(x, y) -> if (x >= 0 and x <= 60 and y >= 0 and y <= 30) then
   State.go Command.Stop init_state else if (x >=65 and x <= 125 and y
   >= 0 and y <= 30) then State.go Command.Draw init_state else if (x
   >=130 and x <= 190 and y >= 0 and y <= 30) then State.go
   Command.EndTurn init_state else init_state *)

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
