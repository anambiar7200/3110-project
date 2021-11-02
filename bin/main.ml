open Game
open Card
open Table
open Player
open Drawing
open State
open Command
open Graphics

let open_window =
  open_graph " 1200x800";
  set_window_title "Rumikub"

let valid_command_form =
  "Please enter a case sensitive valid command with the first word \
   being draw, stop or play followed by run or group and list of \
   cards. cards element in a list of cards must be valid."

let empty_command_message = "You have entered an empty command. "

let malformed_command_message = "This is a malformed command. "

let illegal_move_message = "This move is illegal. "

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

let rec match_command state command =
  let result = go command state in
  match result with
  | Legal new_state -> ask_for_command new_state
  | LegalSwitch st ->
      print_endline endturn_message;
      ask_for_command st
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

let play_game () =
  print_endline "let's play a game >:)";
  init_state |> ask_for_command

let main () =
  let () = open_window in
  set_color black;
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
