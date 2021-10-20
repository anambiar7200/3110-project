(** Representation of dynamic Rummikub state*)

open Card
open Player
open Drawing
open Table
open Command

type state
(** The abstract type of values representing the game state. *)

val init_state : state
(**[init_state] is the initial state after the games starts. 14 cards
   should be dealt to each player. The table should be empty, and the
   deck should be [card_deck] minus the cards dealt to the players*)

val current_deck_lst : state -> Card.card list
(**[current_deck_lst] is the current card list in the deck. type : card
   list*)

val current_player_hand : state -> Player.player
(**[current_player_hand] is the current player's hand. type : player*)

val current_table_lst : state -> Table.table
(**[current_table_lst] is the current table. type : table*)

(** The type representing the result of an attempted movement. *)
type result =
  | Legal of state
  | Illegal

val go : Command.command -> state -> result
(**[go c st] is a result of a player command. The player may choose to
   play, draw a card, or stop the game. If the player's attempted action
   is invalid, an ilegal result will be returned. On the other hand, if
   the player's attempted action is valid, then a legal new state will
   be returned.*)
