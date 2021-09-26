type card = {
  color : string;
  number : int;
  index : int;
  position : string; (**card in position deck, player, table*)
}

(** A modular representation of the player hand,*)
module PlayerHand = struct

  (**[empty] is an empty player's hand*)
  let (empty : card list) = []

  (**[is_empty] represents if the player's hand [hand] is empty*)
  let is_empty (hand : card list) : bool = 
    if hand=empty then true else false

  (**if a player tries play when they have no cards on hand, raise the empty 
  exception*)
  exception Empty
  
  (**[play_card hand c] returns a new card list representing the new player's 
  hand, the new list will not have the card played by the player. Requires 
  [c] is a valid card*)
  let rec play_card (hand : card list) (c : card) = 
    match hand with
    | [] -> []
    | h :: t -> 
        if h.index=c.index then t
        else h :: (play_card t c)

  (**[card_back hand c] returns a new card list in case the player fails to 
  build a new valid set during their turn. Take the card back from the deck. 
  Requires [c] is a valid card *)
  let card_back (hand : card list) (c : card) = c :: hand
  

  (**[draw_card hand c] returns a new card list. Requires [c] is a valid card *)
  let draw_card (hand : card list) (c : card) = c :: hand



end