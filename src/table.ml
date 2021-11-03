open Card

type set_type =
  | Run
  | Group

type set = {
  kind : set_type;
  cards : card list;
}

type table = set list

exception InvalidCombo

exception NoSuchCard

let create_set kind cards = { kind; cards }

let get_cards (st : set) = st.cards

let get_kind (st : set) = st.kind

let create_table sets = sets

let set_size (st : set) = List.length st.cards

let color_equality kind c1 c2 =
  let color1 = get_color c1 in
  let color2 = get_color c2 in
  if color1 = Joker || color2 = Joker then
    if kind = Group then false else true
  else color1 = color2

let number_equality c1 c2 =
  let color1 = get_color c1 in
  let color2 = get_color c2 in
  if color1 = Joker || color2 = Joker then true
  else get_number c1 = get_number c2

let number_succsession c1 c2 =
  let color1 = get_color c1 in
  let color2 = get_color c2 in
  let num1 = get_number c1 in
  let num2 = get_number c2 in
  if num2 = 1 || num1 = 13 then false
  else if color1 = Joker || color2 = Joker then true
  else succ num1 = num2

let rec valid_group set =
  if set.kind = Run then false
  else if List.length set.cards > 4 then false
  else
    match set.cards with
    | [] -> true
    | [ f ] -> true
    | f :: s :: t ->
        if number_equality f s && not (color_equality Group f s) then
          true && valid_group { kind = Group; cards = s :: t }
        else false

let rec valid_run set =
  if set.kind = Group then false
  else
    match set.cards with
    | [] -> true
    | [ f ] -> true
    | f :: s :: t ->
        if color_equality Run f s && number_succsession f s then
          valid_run { kind = Run; cards = s :: t }
        else false

let valid_set set =
  let length = List.length set.cards in
  if length = 0 then true
  else if length >= 3 then valid_run set || valid_group set
  else false

let valid_table table =
  match table with
  | [] -> true
  | _ :: _ ->
      List.fold_left (fun acc x -> acc && valid_set x) true table