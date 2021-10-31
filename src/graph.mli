open Graphics
open Card
open Table

type graph

exception NoMoreSpace

val draw_card : card -> int -> int -> unit
(**[draw_card] draws a card of user choice with its lower left corner at
   (x, y) of length 30 and width 30*)

val init_window : int -> unit
(**[init_window] is the initial window when the game first starts.

   - windoe size: 1200x600
   - title: "Rummikub: CS 3110 Final Project"
   - "Remaining Deck Size" displays the number of cards in the remaining
     deck after dealing 14 cards to each player*)

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

val draw_index : (int * int) * set list list -> color * color
(**[draw_index] returns the (x, y) location info about where to draw a
   new set

   - use after running [add_set]*)

val draw_set : int * int -> int -> int -> set -> unit
(**[draw_set] draws a set of cards at a specific location

   - requires (x, y)
   - number of cards in the set
   - (again) total number of cards in the set
   - the new set you want to add to the table
   - depends on [add_set] and [draw_index] *)
