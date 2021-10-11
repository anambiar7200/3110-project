type player_hand = Card.card list

type player = {
  player_h : player_hand;
  points : int;
}

let empty : player_hand = []

exception OutOfCards
(**if a player tries play when they have no cards on hand, raise the
   OutOfCards exception*)

(**[is_empty] represents if the player's hand [hand] is empty*)
let is_empty (play : player) =
  if play.player_h = empty then true else false

(**[play_card] is the player's hand after playing a card*)
let rec play_card (c : Card.card) (p_h : player_hand) =
  match p_h with
  | h :: t -> if h = c then t else h :: play_card c t
  | empty -> raise OutOfCards

(**[check_ind] shows if the current index is equal to the insertion
   index *)
let check_ind (current : int) (ind : int) =
  if current = ind then true else false

(**[insert_to_table] is a list in the table after the player attempts to
   player a card*)
let rec insert_to_table
    (c : Card.card)
    (table : Card.card list)
    (ind : int)
    (current : int) =
  match table with
  | [] -> [ c ]
  | h :: t ->
      if check_ind current ind then c :: table
      else h :: insert_to_table c t ind (current + 1)

(**[card_back] is the player's hand after the player takes a card back
   from the table*)
let card_back (c : Card.card) (play : player) =
  { player_h = c :: play.player_h; points = play.points }

(**[tale_from_table] is a new combo in the table after the player takes
   back a card they played*)
let rec take_from_table (c : Card.card) (table : Card.card list) =
  match table with
  | [] -> raise Table.NoSuchCard
  | h :: t -> if h = c then t else h :: take_from_table c t

(**[draw_to_player] is a player hand updated from drawing a card from
   the deck using [Drawing.draw]*)
let draw_to_player (play : player) =
  { player_h = Drawing.draw :: play.player_h; points = play.points }

(**[add_pts_player] player with points added to them if they won a round*)
let add_pts_player (play : player) =
  { player_h = play.player_h; points = play.points + 1 }
