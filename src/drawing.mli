type deck
(** the type of deck. *)

exception OutOfCards
(** Raised when there are no more cards left in the deck. *)

val deal : Card.card list
(** [deal] is the cards drawn. *)

val draw : Card.card
(** [draw] is the card drawn. *)

val remaining_deck : Card.card list ref
