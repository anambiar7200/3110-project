open Card
open Table
open Player
open Command

exception NoMoreSpace

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
      if room_in_row h >= set_size st then (h @ [ empty_set; st ]) :: t
      else add_to_row st t

let add_set (st : set) (tb : set list list) =
  if room_in_table tb > 0 then ((which_row tb, 1), add_to_table st tb)
  else (which_ind st tb 1, add_to_row st tb)

let draw_index (tup : (int * int) * set list list) =
  match tup with
  | (r, c), lst -> (((c - 1) * 30) + 150, ((r - 1) * 30) + 100)

let new_table (tup : (int * int) * set list list) =
  match tup with
  | (r, c), lst -> lst

(**[prepend] prepends a card to a set*)
let rec prepend (cd : card) (s : set) =
  match get_cards s with
  | [] -> []
  | h :: t -> [ cd; h ] @ t

(**[append] appends a card to the set*)
let rec append (cd : card) (s : set) =
  match List.rev (get_cards s) with
  | [] -> []
  | h :: t -> List.rev ([ cd; h ] @ t)

let edit_helper (str : string) (cd : card) (s : set) =
  if str = "pre" then create_set (get_kind s) (prepend cd s)
  else if str = "post" then create_set (get_kind s) (append cd s)
  else raise Malformed

(**[insert_to_row] replaces a set list in a table at a specific location*)
let insert_to_row (s_lst : set list) (tb : set list list) (row : int) =
  List.mapi (fun i e -> if i = row then s_lst else e) tb

(**[insert_to_col] replaces a set in a set list at a specific index*)
let insert_to_col (s : set) (s_lst : set list) (col : int) =
  List.mapi (fun i e -> if i = col then s else e) s_lst

let rec replace (tb : set list list) (row : int) (col : int) (s : set) =
  insert_to_row (insert_to_col s (List.nth tb row) col) tb row
