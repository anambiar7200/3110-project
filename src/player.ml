type deck_player_table = string

type card_lst_ind = int

type card_ind_in_lst = int

type position = {
  location : deck_player_table;
      (**whether the card is in the deck, player's hand, or on the table*)
  list_ind : card_lst_ind;  (**index of the card list*)
  card_ind : card_ind_in_lst;  (**index of the card in the list*)
}

type card = {
  color : string;
  number : int;
  index : int;
  pos : position;
}

(** A modular representation of the player hand,*)
module PlayerHand = struct
  type hand = card list

  (**[empty] is an empty player's hand*)
  let (empty : card list) = []

  (**[is_empty] represents if the player's hand [hand] is empty*)
  let is_empty (hand : card list) = function
    | [] -> true
    | _ -> false

  exception Empty
  (**if a player tries play when they have no cards on hand, raise the
     empty exception*)

  (**[new_pos_info] updates a position*)
  let new_pos_info
      (c : card)
      (loc : deck_player_table)
      (cli : card_lst_ind)
      (ci : card_ind_in_lst) =
    { location = loc; list_ind = cli; card_ind = ci }

  (**[new_card] is a new card with new position*)
  let new_card (c : card) (p : position) =
    { color = c.color; number = c.number; index = c.index; pos = p }

  (**[update_tail_card_lst] increment the card index of the cards in the
     tail card list by 1. helper function for [card_inset]*)
  let rec update_tail_card_lst (cl : card list) =
    match cl with
    | [] -> []
    | h :: t ->
        new_card h
          (new_pos_info h h.pos.location h.pos.list_ind
             (h.pos.card_ind + 1))
        :: update_tail_card_lst t

  (**[card_insert] inserts [c] to a card list [cl] based on [c]'s
     postion*)
  let rec card_insert (c : card) (cl : card list) =
    match cl with
    | [] -> [ c ]
    | h :: t ->
        if h.pos = c.pos then c :: update_tail_card_lst cl
        else h :: card_insert c t

  (**[update_card_position] updates the card's position bassed on user
     choice. It returns a new card with the old card information but new
     postion. The card will be appended to the destination card list*)
  let update_card_position (c : card) (p : position) (cl : card list) =
    { color = c.color; number = c.number; index = c.index; pos = p }
    :: cl

  (**[play_card hand c] returns a new card list representing the new
     player's hand, the new list will not have the card played by the
     player. Requires [c] is a valid card*)
  let rec play_card (hand : card list) (c : card) =
    match hand with
    | [] -> []
    | h :: t -> if h.index = c.index then t else h :: play_card t c

  (**[card_back hand c] returns a new card list in case the player fails
     to build a new valid set during their turn. Take the card back from
     the deck. Requires [c] is a valid card *)
  let card_back (hand : card list) (c : card) (p : position) =
    update_card_position c p hand

  (**[draw_card hand c] returns a new card list. Requires [c] is a valid
     card *)
  let draw_card (hand : card list) (c : card) (p : position) =
    update_card_position c p hand
end