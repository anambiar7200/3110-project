open Graphics
open Card
open Table

type graph

val card_color : card -> color

val draw_card : card -> int -> int -> unit
(**[draw_card] draws a card of user choice with its lower left corner at
   (x, y) of length 30 and width 30*)

val init_window : int -> unit
(**[init_window] is the initial window when the game first starts.

   - windoe size: 1200x600
   - title: "Rummikub: CS 3110 Final Project"
   - "Remaining Deck Size" displays the number of cards in the remaining
     deck after dealing 14 cards to each player*)

val draw_set : int * int -> int -> int -> set -> unit
(**[draw_set] draws a set of cards at a specific location

   - requires (x, y)
   - number of cards in the set
   - (again) total number of cards in the set
   - the new set you want to add to the table
   - depends on [add_set] and [draw_index] *)
