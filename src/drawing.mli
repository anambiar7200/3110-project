type deck
(** the type of deck. *)

exception OutOfCards
(** Raised when there are no more cards left in the deck. *)

val deal : Card.card list -> Card.card list * Card.card list
(** [deal] is the cards drawn. *)

val draw : Card.card list -> Card.card * Card.card list
(** [draw] is the card drawn. *)

val drawing_init : unit -> Card.card list
(** [drawing_init] is the shuffled deck. *)