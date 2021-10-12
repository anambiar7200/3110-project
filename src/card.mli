type color_type
(** The type of color. *)

type position_type
(** The type of card position. *)

type card
(** The type of card. *)

val create_card :
  int -> int -> color_type -> int -> card list -> card list

val card_deck : card list
(** card_deck is list of 104 rummikub cards*)