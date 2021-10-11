type player_hand
(** The abstract type of values representing the player's hand. *)

type player
(** The abstract type of values representing the player's profile. *)

val empty : player_hand

exception OutOfCards
(**if a player tries play when they have no cards on hand, raise the
   OutOfCards exception*)

val is_empty : player -> bool
(**[is_empty] represents if the player's hand [player] is empty*)

val play_card : Card.card -> player_hand -> player_hand
(**[play_card] is the player's hand after playing a card*)

val draw_to_player : player -> player
(**[draw_to_player] is a player profile updated from drawing a card from
   the deck*)

val add_pts_player : player -> player
(**[add_pts_player] player with points added to them, if they won a
   round*)
