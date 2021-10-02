(** card in position deck, player, table
color should be a variant*)


type card = {
  number : int;
  color : string;
  index : int;
  position: string; 
}

type joker = {
index : int;
}


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

type color = Black | Blue | Orange | Red
type position = Deck | Player | Table
