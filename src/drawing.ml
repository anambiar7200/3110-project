open Card

exception OutOfCards

let bound = 1000

let cards_per_person = 14

let random_seed = Sys.time () |> int_of_float |> Random.init

let make_tuple card = (Random.int bound, card)

let tuple_compare a b =
  if fst a = fst b then 0 else if fst a > fst b then 1 else -1

let de_tuple tuple = snd tuple

let shuffle deck =
  random_seed;
  deck |> List.map make_tuple |> List.sort tuple_compare
  |> List.map de_tuple

let drawing_init () = shuffle card_deck

let rec deal_helper acc needed deck : card list * card list =
  if needed = 0 then (acc, deck)
  else
    match deck with
    | [] -> raise OutOfCards
    | h :: t -> deal_helper (h :: acc) (needed - 1) t

let deal (deck : card list) = deal_helper [] cards_per_person deck

let draw deck : card * card list =
  match deck with
  | [] -> raise OutOfCards
  | h :: t -> (h, t)