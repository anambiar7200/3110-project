type color_type

type position_type

type card

val create_card :
  int -> int -> color_type -> int -> card list -> card list

val card_deck : card list

val get_number : card -> int

val get_color : card -> color_type

val get_index : card -> int
