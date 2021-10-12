(*open Card
let card1 = {number = 1; color = Blue; index = 0}
let num1 = card1.number
*)
type set_type = 
  | Run
  | Group

type set = {
  kind: set_type; 
  cards: Card.card list; 
}

type table = set list

exception InvalidCombo

exception NoSuchCard

let check_valid (tb : table) = true

let rec valid_group cards = 
  match cards with 
  | _ :: _ :: [] -> false
  | f :: s :: t :: [] -> ((f.number = s.number) && (s.number = t.number)) &&
    ((f.color <> s.color) && (s.color <> t.color) && (f.color <> t.color))
  | f :: s :: tl -> if (f.number = s.number) && (f.color <> s.color) then 
    true && valid_group s::tl else false

let valid_run set = 
  if set.kind = Group then false else
  match set.cards with 
  | _ :: _ :: [] -> false
  | f :: s :: t :: []-> if ((f.color = s.color) && (f.color = t.color)) then 
    ((succ f.number = s.number) && (succ s.number = t.number))
  | f :: s :: tl -> if (f.color = s.color) && (succ f.number = s.number) then 
    valid_inc_run s :: tl else false

let valid_set set = valid_run set || valid_group set.cards; 
