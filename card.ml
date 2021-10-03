(** card in position deck, player, table
color should be a variant
joker1 =  {0 joker 104 deck}
joker2 = {0 joker 105 deck}*)


type color_type = Black | Blue | Orange | Red | Joker
type position_type = Deck | Player | Table


type card = {
  number : int;
  color : color_type;
  index : int;
  position: position_type; 
}


(** create_card max_number number_in color_in index_in card_list outputs a list 
of cards with 1 to max_number 4 colors black blue orange red and index from 0 to  *)
let rec create_card (max_number: int )(number_in: int)(color_in : string) (index_in : int) (card_list : card list)  = 
  let new_card = {number =  number_in; color = color_in; index = index_in; position = "deck" } in
  let new_card_list = card_list @ [new_card] in
  match List.length new_card_list with 
  | i when i = max_number -> create_card max_number 1 "black" max_number new_card_list
  | i when i = max_number*2 -> create_card max_number 1 "blue" (max_number*2) new_card_list
  | i when i = max_number*3 -> create_card max_number 1 "blue" (max_number*3) new_card_list
  | i when i = max_number*4 -> create_card max_number 1 "orange" (max_number*4) new_card_list
  | i when i = max_number*5 -> create_card max_number 1 "orange" (max_number*5) new_card_list
  | i when i = max_number*6 -> create_card max_number 1 "red" (max_number*6) new_card_list
  | i when i = max_number*7 -> create_card max_number 1 "red" (max_number*7) new_card_list
  | i when i = max_number*8 -> new_card_list
  | i -> create_card max_number (number_in +1) color_in (index_in+1) new_card_list

let card_list = create_card 13 1 "black" 0 []
