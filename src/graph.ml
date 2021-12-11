open Graphics
open Card
open Table
open Player
open Add
open State

type graph = Graphics

(**matching card color to graphics color*)
let card_color (cd : card) =
  match get_color cd with
  | Black -> Graphics.black
  | Orange -> rgb 250 140 0
  | Red -> Graphics.red
  | Blue -> Graphics.blue
  | Joker -> Graphics.green

(* let selected (cd : card) (x : int) (y : int) = moveto x y; draw_rect
   x y 30 30; moveto (x + 13) (y + 10); fill_rect set_color (card_color
   cd); draw_string (string_of_int (get_number cd)); set_color
   Graphics.black; if get_color cd = Joker then draw_circle (x + 15) (y
   + 15) 10 *)

(**each card's dimension is 30x30*)
let draw_card (cd : card) (x : int) (y : int) =
  moveto x y;
  draw_rect x y 30 30;
  moveto (x + 13) (y + 10);
  set_color (card_color cd);
  draw_string (string_of_int (get_number cd));
  set_color Graphics.black;
  if get_color cd = Joker then draw_circle (x + 15) (y + 15) 10

(**[draw_hide_card] draws the hidden next player's hand*)
let draw_hide_card (cd : card) (x : int) (y : int) =
  moveto x y;
  set_color (rgb 192 192 192);
  draw_rect x y 30 30;
  set_color Graphics.black

(** - First row: f initially, i = 0, size = 30, then the function
      returns the first 30 cards in the player's hand
    - Second row: Else if initially, i = 30 and size>30, then the
      function returns all the cards after index 30 in player's hand
    - First column: f initially, i = 0, size = 15, then the function
      returns the first 15 cards in the player's hand
    - Second column: Else if initially, i = 15 and size>15, then the
      function returns all the cards after index 15 in player's hand
    - same rule apply*)
let rec seperate_player
    (pl : player)
    (i : int)
    (size : int)
    (acc : card list) =
  if i < size then
    let new_acc = acc @ [ List.nth pl i ] in
    seperate_player pl (i + 1) size new_acc
  else acc

(**check if the current player hand is overflowing the screen
   horizontally*)
let check_current_hand_overflow (pl : player) =
  let size = player_size pl in
  if size > 30 then true else false

let check_next_hand_overflow (pl : player) =
  let size = player_size pl in
  if size > 15 then true else false

(**[draw_current_player] draws the current player hand at a coordinate*)
let rec draw_current_player (pl : player) (x : int) (y : int) =
  let over_flow = check_current_hand_overflow pl in
  if over_flow then (
    let first, rest =
      ( seperate_player pl 0 30 [],
        seperate_player pl 30 (player_size pl) [] )
    in
    draw_current_player first x y;
    draw_current_player rest x (y - 30))
  else
    match pl with
    | [] -> moveto 0 0
    | h :: t ->
        draw_card h x y;
        draw_current_player t (x + 30) y

(**[draw_next_player] draws the next player hand, hiding their number
   and color*)
let rec draw_next_player (pl : player) (x : int) (y : int) =
  let over_flow = check_next_hand_overflow pl in
  if over_flow then (
    let first, rest =
      ( seperate_player pl 0 15 [],
        seperate_player pl 15 (player_size pl) [] )
    in
    draw_next_player first x y;
    draw_next_player rest (x + 30) y)
  else
    match pl with
    | [] -> moveto 0 0
    | h :: t ->
        draw_hide_card h x y;
        draw_next_player t x (y - 30)

let draw_error_message (m : string) =
  moveto 20 580;
  set_color red;
  draw_string m;
  set_color black

let init_window (num_of_pl : int) =
  open_graph " 1200x600";

  set_window_title "Rummikub: CS 3110 Final Project";
  draw_rect 0 0 35 30;
  moveto 5 10;
  set_color (rgb 153 0 0);
  draw_string "STOP";
  set_color Graphics.black;
  draw_rect 40 0 35 30;
  moveto 45 10;
  set_color (rgb 51 0 102);
  draw_string "DRAW";
  set_color Graphics.black;
  draw_rect 80 0 55 30;
  moveto 85 10;
  set_color (rgb 102 102 0);
  draw_string "ENDTURN";
  set_color Graphics.black;
  moveto 1055 20;
  draw_string ("Deck Size: " ^ string_of_int (106 - (14 * num_of_pl)));
  draw_current_player (current_player_hand init_state) 135 65;
  draw_next_player (current_next_player init_state) 15 550;
  moveto 500 5;
  draw_string "Player 1's turn"

let rec draw_set
    ((x, y) : int * int)
    (num : int)
    (total : int)
    (st : set) =
  match num with
  | 0 -> moveto 0 0
  | n ->
      draw_card (List.nth (get_cards st) (total - n)) x y;
      draw_set (x + 30, y) (num - 1) total st
