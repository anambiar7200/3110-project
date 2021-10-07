(* type color_type = Black | Blue | Orange | Red | Joker type
   position_type = Deck | Player | Table

   type card = { number : int; color : color_type; index : int;
   position: position_type; }

   module Dealing = struct type deck = card list let remaining_deck =

   (** [shuffle deck] is the shuffled deck. Requires: [deck] is a valid
   deck. *) let shuffle (deck:deck) =

   (** [ideal_to_player player] is the list of 14 cards drawn to player
   [player]. Requires: [player] is a valid player. *) let deal_to_player
   player =

   (** [draw player] is the card drawn to player [player]. Requires:
   [player] is a valid player. *) let draw player =

   (** [allow_to_draw deck] is true or false. It represents whether or
   not there's eough cards i the deck to draw. Requires: [deck] is a
   valid deck. *) let allow_to_draw deck =

   end *)
