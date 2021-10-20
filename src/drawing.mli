exception OutOfCards
(** Raised when there are no more cards left in the deck. *)

val deal : Card.card list -> Card.card list * Card.card list
(** [deal] is the tuple of (14 drawn cards in a card list * remaining
    deck). *)

val draw : Card.card list -> Card.card * Card.card list
(** [draw] is the tuple of (card * remaining deck). *)

val drawing_init : unit -> Card.card list
(** [drawing_init] is the shuffled deck. *)