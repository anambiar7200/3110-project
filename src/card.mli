(** The type of color. Joker has a separate color because it can be used
    as any color *)
type color_type =
  | Black
  | Blue
  | Orange
  | Red
  | Joker

(** The type of card position. *)
type position_type =
  | Deck
  | Player
  | Table

type card
(** The type of card. *)

val get_number : card -> int
(** [get_number c] is the number of a card c. *)

val card_deck : card list

val get_color : card -> color_type
(** [get_color c] is the color of a card c. *)

val get_index : card -> int
(** [get_index c] is the index of a card c. *)

val card_deck : card list
(** card_deck is a list of 104 rummikub cards. *)
