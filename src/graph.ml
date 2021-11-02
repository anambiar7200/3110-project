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

(**[draw_current_player] draws the current player hand at a coordinate*)
let rec draw_current_player (pl : player) (x : int) (y : int) =
  match pl with
  | [] -> moveto 0 0
  | h :: t ->
      draw_card h x y;
      draw_current_player t (x + 30) y

(**[draw_next_player] draws the next player hand, hiding their number
   and color*)
let rec draw_next_player (pl : player) (x : int) (y : int) =
  match pl with
  | [] -> moveto 0 0
  | h :: t ->
      draw_hide_card h x y;
      draw_next_player t x (y - 30)

let init_window (num_of_pl : int) =
  open_graph " 1200x600";

  set_window_title "Rummikub: CS 3110 Final Project";
  draw_rect 0 0 60 30;
  moveto 13 10;
  set_color (rgb 153 0 0);
  draw_string "STOP";
  set_color Graphics.black;
  draw_rect 65 0 60 30;
  moveto 78 10;
  set_color (rgb 51 0 102);
  draw_string "DRAW";
  set_color Graphics.black;
  draw_rect 130 0 60 30;
  moveto 143 10;
  set_color (rgb 102 102 0);
  draw_string "ENDTURN";
  set_color Graphics.black;
  moveto 1000 20;
  draw_string
    ("Remaining Deck Size: " ^ string_of_int (106 - (14 * num_of_pl)));
  draw_current_player
    (State.current_player_hand State.init_state)
    150 50;
  draw_next_player (State.current_next_player State.init_state) 50 540

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
