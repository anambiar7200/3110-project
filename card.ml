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
  let newcard = {number =  number_in; color = color_in; index = index_in; position = "deck" } in
  let new_card_list = card_list @ [newcard] in
  match List.length new_card_list with 
  | i when i = max_number -> create_card max_number 1 "black" 13 new_card_list
  | i when i = max_number*2 -> create_card max_number 1 "blue" (13*2) new_card_list
  | i when i = max_number*3 -> create_card max_number 1 "blue" (13*3) new_card_list
  | i when i = max_number*4 -> create_card max_number 1 "orange" (13*4) new_card_list
  | i when i = max_number*5 -> create_card max_number 1 "orange" (13*5) new_card_list
  | i when i = max_number*6 -> create_card max_number 1 "red" (13*6) new_card_list
  | i when i = max_number*7 -> create_card max_number 1 "red" (13*7) new_card_list
  | i when i = max_number*8 -> new_card_list
  | i -> create_card max_number (number_in +1) color_in (index_in+1) new_card_list

let card_list = create_card 13 1 "black" 0 []
  
  

  
  
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
      (** 



    let rec create_card (number_in: int) (color_in : string) (index_in : int) (card_list : card list)  = 

    if (List.length card_list = 13) then create_card 1 "black" 13 card_list
    else 
      (let newcard = {number =  number_in; color = color_in; index = index_in; position = "deck" } in
      let new_card_list = newcard::card_list in
      create_card (number_in +1) color_in (index_in+1) new_card_list)
  

  
    else if number_in < 0 && (List.length card_list = number_in) then create_card 13 color_in (index_in-1) (new_card_list))
  else 
  (let newcard = {number =  number_in; color = color_in; index = index_in; position = "deck" } in
  let new_card_list = newcard::card_list in
  create_card (number_in-1) color_in (index_in-1) (new_card_list))
  *)




(** input greatest number on card, list of colors and number of sets and outputs 
list of cards, cards in dex
create_card 13 ["black", "blue", "orange", "red"] 2
each card is {color = "R"; number = 1; index = 0; position = "deck"}
*)

(** 
let rec create_card (number_in: int) (color_in : string) (set_in: int) = 

  index = 
  if number_in <= 13 then {number = number_in; color = color_in; index = }
    create index after recursion? like index of element as card




  a2 testing
  
  make card list for each color 
  
  
  index = 
  if number_in <= 13 then {number = number_in; color = color_in; index = }
    create index after recursion? like index of element as card




  a2 testing

  case 1 after 1 set is over 13 cards
  case 2 after 2 set is over 26 cards


#version 2 call function each time for different color, so 4 times 
let rec create_card (number_in: int) (color_in : string) (index_in : int) (card_list : card list)  = 
  if number_in < 0 && (List.length card_list = number_in*2) then card_list
  else if number_in < 0 && (List.length card_list = number_in) then create_card 13 color_in (index_in-1) (new_card_list))
  else 
  (let newcard = {number =  number_in; color = color_in; index = index_in; position = "deck" } in
  let new_card_list = newcard::card_list in
  create_card (number_in-1) color_in (index_in-1) (new_card_list))

  *)



