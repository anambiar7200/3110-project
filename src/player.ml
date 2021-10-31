open Card
open Table

type player = Card.card list

let empty : player = []

exception OutOfCards
(**if a player tries play when they have no cards on hand, raise the
   OutOfCards exception*)

exception NotYourCard

(**[is_empty] represents if the player's hand [hand] is empty*)
let is_empty (play : player) = if play = [] then true else false

let build_player (clst : Card.card list) : player = clst

let player_size (pl : player) = List.length pl

let peek_player (p : player) =
  match p with
  | [] -> raise OutOfCards
  | h :: t -> h

let rec play_card2 (index : int) (p_h : player) : player =
  match p_h with
  | h :: t -> if get_index h = index then t else h :: play_card2 index t
  | empty -> raise OutOfCards

(**[check_ind] shows if the current index is equal to the insertion
   index *)
let check_ind (current : int) (ind : int) =
  if current = ind then true else false

let rec insert_to_list
    (c : Card.card)
    (table : Card.card list)
    (ind : int)
    (current : int) =
  (*current start with 0*)
  match table with
  | [] -> [ c ]
  | h :: t ->
      if check_ind current ind then c :: table
      else h :: insert_to_list c t ind (current + 1)

let insert_to_set (c : Card.card) (st : set) (ind : int) (current : int)
    =
  create_set (get_kind st) (insert_to_list c (get_cards st) ind current)

let card_back (c : Card.card) (play : player) (before : player) : player
    =
  match List.mem c before with
  | true -> c :: play
  | false -> raise NotYourCard

let rec take_from_list
    (c : Card.card)
    (table : Card.card list)
    (before : player) =
  match List.mem c before with
  | false -> raise NotYourCard
  | true -> (
      match table with
      | [] -> raise Table.NoSuchCard
      | h :: t -> if h = c then t else h :: take_from_list c t before)

let take_from_set (c : Card.card) (st : set) (before : player) =
  create_set (get_kind st) (take_from_list c (get_cards st) before)

let add_to_player (play : player) (c : Card.card) : player = c :: play

let player_compare (p1 : player) (p2 : player) =
  List.sort_uniq (fun x y -> get_index x - get_index y) p1
  = List.sort_uniq (fun x y -> get_index x - get_index y) p2