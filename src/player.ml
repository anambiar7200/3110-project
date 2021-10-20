type player = Card.card list

let empty : player = []

exception OutOfCards
(**if a player tries play when they have no cards on hand, raise the
   OutOfCards exception*)

exception NotYourCard

(**[is_empty] represents if the player's hand [hand] is empty*)
let is_empty (play : player) = if play = [] then true else false

let build_player (clst : Card.card list) : player = clst

let peek_player (p : player) =
  match p with
  | [] -> raise OutOfCards
  | h :: t -> h

let rec show_player_hand (p : player) =
  match p with
  | [] -> []
  | h :: t -> h :: show_player_hand t

let rec play_card (c : Card.card) (p_h : player) : player =
  match p_h with
  | h :: t -> if h = c then t else h :: play_card c t
  | empty -> raise OutOfCards

(**[check_ind] shows if the current index is equal to the insertion
   index *)
let check_ind (current : int) (ind : int) =
  if current = ind then true else false

let rec insert_to_table
    (c : Card.card)
    (table : Card.card list)
    (ind : int)
    (current : int) =
  (*current start with 0*)
  match table with
  | [] -> [ c ]
  | h :: t ->
      if check_ind current ind then c :: table
      else h :: insert_to_table c t ind (current + 1)

let player_create_set (clst : Card.card list) (kd : Table.set_type) =
  Table.create_set kd clst

let card_back (c : Card.card) (play : player) (before : player) : player
    =
  match List.mem c before with
  | true -> c :: play
  | false -> raise NotYourCard

let rec take_from_table
    (c : Card.card)
    (table : Card.card list)
    (before : player) =
  match List.mem c before with
  | false -> raise NotYourCard
  | true -> (
      match table with
      | [] -> raise Table.NoSuchCard
      | h :: t -> if h = c then t else h :: take_from_table c t before)

let add_to_player (play : player) (c : Card.card) : player = c :: play
