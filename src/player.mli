type player = Card.card list
(** The abstract type of values representing the player's hand. *)

val empty : player

exception OutOfCards
(**if a player tries play when they have no cards on hand, raise the
   OutOfCards exception*)

exception NotYourCard
(**if a player tries to take back a card that did not belong to them,
   raise NotYourCard*)

val is_empty : player -> bool
(**[is_empty] represents if the player's hand [player] is empty*)

val build_player : Card.card list -> player
(**[build_player] is a player's hand built from a list of cards*)

val peek_player : player -> Card.card
(**[peek_player] is the first card in the player's hand from left to
   right*)

val play_card : Card.card -> player -> player
(**[play_card] is the player's hand after playing a card*)

val play_card2 : int -> player -> player
(**[play_card] is the player's hand after playing a card*)

val insert_to_table :
  Card.card -> Card.card list -> int -> int -> Card.card list
(**[insert_to_table] is a new set in the table after the player attempts
   to player a card*)

val take_from_table :
  Card.card -> Card.card list -> player -> Card.card list
(**[tale_from_table] is a new set in the table after the player takes
   back a card they played*)

val card_back : Card.card -> player -> player -> player
(**[card_back] is the player's hand after the player takes a card back
   from the table*)

val add_to_player : player -> Card.card -> player
(**[add_to_player] is a player's hand updated from drawing a card from
   the deck*)

val player_compare : player -> player -> bool
(** [player_compare p1 p2] returns true if the player hands are equal*)
