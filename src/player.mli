type player
(** The abstract type of values representing the player's hand. *)

val empty : player

exception OutOfCards
(**if a player tries play when they have no cards on hand, raise the
   OutOfCards exception*)

val is_empty : player -> bool
(**[is_empty] represents if the player's hand [player] is empty*)

val build_player : Card.card list -> player
(**[build_player] is a player's hand built from a list of cards*)

val play_card : Card.card -> player -> player
(**[play_card] is the player's hand after playing a card*)

val card_back : Card.card -> player -> player
(**[card_back] is the player's hand after the player takes a card back
   from the table*)

val draw_to_player : player -> player
(**[draw_to_player] is a player profile updated from drawing a card from
   the deck*)
