open Graphics
open Card
open Table

type graph = Graphics

exception NoMoreSpace

(**matching card color to graphics color*)
let card_color (cd : card) =
  match get_color cd with
  | Black -> Graphics.black
  | Orange -> rgb 250 140 0
  | Red -> Graphics.red
  | Blue -> Graphics.blue
  | Joker -> Graphics.green

(**each card's dimension is 30x30*)

let draw_card (cd : card) (x : int) (y : int) =
  moveto x y;
  draw_rect x y 30 30;
  moveto (x + 13) (y + 10);
  set_color (card_color cd);
  draw_string (string_of_int (get_number cd));
  set_color Graphics.black;
  if get_color cd = Joker then draw_circle (x + 15) (y + 15) 10

let color_select_card (cd : card) (x : int) (y : int) =
  moveto x y;
  draw_rect x y 30 30;
  set_color Graphics.yellow;
  fill_rect x y 30 30;
  moveto (x + 13) (y + 10);
  set_color (card_color cd);
  draw_string (string_of_int (get_number cd));
  set_color Graphics.black;
  if get_color cd = Joker then draw_circle (x + 15) (y + 15) 10

(** keep track of every card index *)
let init_window (num_of_pl : int) =
  open_graph " 1200x600";

  set_window_title "Rummikub: CS 3110 Final Project";

  moveto 1000 20;
  draw_string
    ("Remaining Deck Size" ^ string_of_int (106 - (14 * num_of_pl)))

let table_width = 900

let table_height = 450

let row_limit = 30

let col_limit = 15

let player_max = 25

(**[sum_of_sets] is the number of slots in a row taken by a list of sets*)
let rec sum_of_sets (st_lst : set list) =
  match st_lst with
  | [] -> 0
  | h :: t ->
      if set_size h = 0 then 1 + sum_of_sets t
      else set_size h + sum_of_sets t

(**[room_in_row] is the remaining number of spots in a row*)
let room_in_row (st_lst : set list) =
  if List.length st_lst = 1 then row_limit - sum_of_sets st_lst - 1
  else row_limit - sum_of_sets st_lst

(**[room_in_col] is the remaining number of rows not filled*)
let room_in_table (tb : set list list) = col_limit - List.length tb

(**empty space between 2 seperating different sets in a row*)
let empty_set : set = create_set Run []

(**[add_to_table] attempts to add a new set to one of the rows in the
   table

   - this function assumes NOT all the rows in the table have been
     filled
   - adds a new set to a new row in the table*)
let rec add_to_table (st : set) (tb : set list list) =
  match tb with
  | [] -> [ [ st ] ]
  | h :: t -> h :: add_to_table st t

(**[add_to_row] attempts to add a new set to one of the rows in the
   table

   - this function assumes all the rows in the table have been filled
   - adds a new set after an old set in a row, seperated by an empty set
     / space
   - if the table does not have enough space, raise [NoMoreSpace]**)
let rec add_to_row (st : set) (tb : set list list) =
  match tb with
  | [] -> raise NoMoreSpace
  | h :: t ->
      if room_in_row h >= set_size st then
        List.rev (st :: empty_set :: h) :: t
      else add_to_row st t

(**[which_row] returns the index of the row of the new set, index starts
   with 1*)
let rec which_row (tb : set list list) =
  match tb with
  | [] -> 1
  | h :: t -> 1 + which_row t

(**[which_ind] returns the index of the row and the row index of the new
   set

   - (index of row * row index)
   - if the new set cannot fit in the table, raise [NoMoreSpace]*)
let rec which_ind (st : set) (tb : set list list) (ind : int) =
  match tb with
  | [] -> raise NoMoreSpace
  | h :: t ->
      if room_in_row h >= set_size st then
        if List.length tb = 1 then (ind, sum_of_sets h + 3)
        else (ind, sum_of_sets h + 2)
      else which_ind st t (ind + 1)

let add_set (st : set) (tb : set list list) =
  if room_in_table tb > 0 then ((which_row tb, 1), add_to_table st tb)
  else (which_ind st tb 1, add_to_row st tb)

let draw_index (tup : (int * int) * set list list) =
  match tup with
  | (r, c), lst -> (((c - 1) * 30) + 150, ((r - 1) * 30) + 100)

let rec draw_set
    ((x, y) : int * int)
    (num : int)
    (total : int)
    (st : set) =
  match num with
  | 0 -> moveto 0 0
  | n ->
      draw_card (List.nth (get_cards st) (total - n)) x y;
      draw_set (x + 30, y) (num - 1) total st;

(** [get_card_pos] input card index output card position from association list*)

((** Question: Does calling new_set on same set multiple times change the index 
of the set? *))
let set_position (st : set) (tb : set list list)  =
  let new_set = get_cards st in
  let newsetpos_table = add_set st tb in
  let first_card_pos = draw_index newsetpos_table in
  (new_set, first_card_pos)

let rec card_position (new_set : card list) (first_card_pos: int * int) (card_pos_list: (int * (int*int)) list)  =
  match new_set with 
  | h :: t   -> (
  let first_card = List.nth new_set 0 in
  let first_card_ind = get_index first_card in
  let new_card_pos_list = card_pos_list @ [(first_card_ind, first_card_pos)] in  
  card_position t 
  ((fst first_card_pos) + 30, (snd first_card_pos)) new_card_pos_list ) 
  | [] -> card_pos_list 
