open Game
open Card
open Table
open Player
open Drawing

let valid_command_form =
  "Please enter a valid command (case sensitive): "

let empty_command_message =
  "You have entered an empty command. " ^ valid_command_form

let invalid_command_message =
  "The format of this command is invalid. " ^ valid_command_form

let byebye_message = "Thank you for playing this game. Bye!"

let next_command_message =
  "Please indicate which feature of this module you would like to see. \
   If you wish to view the feature of another model, please type out \
   the name of that module (case sensitive)."

let play_game f = print_endline "let's play a game >:)"

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n\
     Welcome to MS1 for the Rummikub Game Engine (CS 3110 Final \
     Project).\n";
  print_endline
    "Authors: Ja Young Byun (jb2297), Yuyi He (yh383), Danyu Hu \
     (dh573), Anusha Nambiar (aan29)";
  print_endline
    "Please enter 'start game' (Case sensitive) to start Rummikub and \
     view our demo: \n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | response -> play_game response

let () = main ()