open Game
open Card
open Table
open Player
open Drawing
open State
open Command

let valid_command_form =
  "Please enter a case sensitive valid command with the first word \
   being draw, stop or play followed by run or group and list of \
   cards. cards element in a list of cards must be valid."

let empty_command_message =
  "You have entered an empty command. " ^ valid_command_form

let invalid_command_message =
  "The format of this command is invalid. " ^ valid_command_form

let fairewell_message = "Thank you for playing this game. Bye!"

let get_color_str color_in =
  match color_in with
  | Black -> "Black"
  | Blue -> "Blue"
  | Orange -> "Orange"
  | Red -> "Red"
  | Joker -> "Joker"

let print_card c =
  let number_str = string_of_int (get_number c) in
  let color_str = get_color_str (get_color c) in
  let index_str = string_of_int (get_index c) in
  "{ number: " ^ number_str ^ "color: " ^ color_str ^ "index: "
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
  let result = State.go command state in
  match result with
  | Legal new_state -> ask_for_command new_state
  | Illegal ->
      print_endline invalid_command_message;
      ask_for_command state

and ask_for_command state =
  print_list (current_player_hand state);
  print_endline valid_command_form;
  let command = read_line () in
  try parse_input command |> match_command state with
  | Empty ->
      print_endline empty_command_message;
      ask_for_command state
  | Malformed ->
      print_endline invalid_command_message;
      ask_for_command state

let play_game () =
  print_endline "let's play a game >:)";
  init_state |> ask_for_command

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
