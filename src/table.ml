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

let rec valid_group cards =
  match cards with
  | [] -> false
  | [ _ ] -> false
  | [ _; _ ] -> false
  | [ f; s; t ] ->
      (get_number f = get_number s && get_number s = get_number t)
      && get_color f <> get_color s
      && get_color s <> get_color t
      && get_color f <> get_color t
  | f :: s :: tl ->
      if get_number f = get_number s && get_color f <> get_color s then
        true && valid_group (s :: tl)
      else false

let rec valid_run set =
  if set.kind = Group then false
  else
    match set.cards with
    | [] -> false
    | [ _ ] -> false
    | [ _; _ ] -> false
    | [ f; s; t ] ->
        if get_color f = get_color s && get_color f = get_color t then
          succ (get_number f) = get_number s
          && succ (get_number s) = get_number t
        else false
    | f :: s :: tl ->
        if
          get_color f = get_color s
          && succ (get_number f) = get_number s
        then valid_run { kind = Run; cards = s :: tl }
        else false

let valid_set set = valid_run set || valid_group set.cards
