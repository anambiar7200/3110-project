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

let create_table sets = sets

let rec valid_group set =
  if set.kind = Run then false
  else
    match set.cards with
    | [] -> false
    | [ _ ] -> false
    | [ _; _ ] -> false
    | [ f; s; t ] ->
        (get_number f = get_number s && get_number s = get_number t)
        && get_color f <> get_color s
        && get_color s <> get_color t
        && get_color f <> get_color t
    | f :: s :: tl ->
        if get_number f = get_number s && get_color f <> get_color s
        then true && valid_group { kind = Group; cards = s :: tl }
        else false

let rec valid_run set =
  if set.kind = Group then false
  else if List.length set.cards < 3 then false
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

let valid_set set = valid_run set || valid_group set

let valid_table table =
  match table with
  | [] -> true
  | _ :: _ ->
      List.fold_left (fun acc x -> acc && valid_set x) true table