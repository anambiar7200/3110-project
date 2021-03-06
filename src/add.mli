open Card
open Table
open Player

exception NoMoreSpace

val table_width : int
(**window width reserved for the table : 900*)

val table_height : int
(**window height reserved for the table : 450*)

val row_limit : int
(**limit for the absolute number of cards in a set: table_width/30 : 30*)

val col_limit : int
(**limit for the maximum number of columns in the table: table_height/30
   : 15*)

val player_max : int
(**Limit for the maximum number of cards in the player's hand: 25*)

val empty_set : set

val add_set : set -> set list list -> (int * int) * set list list
(**[add_set] attempts to add a new set to a table/set list list

   - it fills all the rows in the tables one by one first
   - if there are unfilled rows, we add the new set to the next unfilled
     row
   - if there are no empty row left, we attempt to add the new set to
     one of the filled rows
   - if there is not enough space in the table, raise [NoMoreSpace]
   - returns ((x, y), table_lst) where (x, y) is the new coordinate to
     add the new set*)

val draw_index : (int * int) * set list list -> int * int
(**[draw_index] returns the (x, y) location info about where to draw a
   new set

   - use after running [add_set]*)

val new_table : (int * int) * set list list -> set list list
(**[new_table] retuns the new table/set list list after adding a new set
   to the table*)

val edit_helper : string -> card -> set -> set
(**[edit_helper] either prepends or appends a card to a set based on
   user choice to a set, and it reaturns a new set*)

val replace : set list list -> int -> int -> set -> set list list
(**[replace] replaces a set in the table at a specific location, returns
   the new table*)
