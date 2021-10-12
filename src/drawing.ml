open Card

type deck = card list

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

let remaining_deck = ref (shuffle card_deck)

let rec deal_helper acc needed =
  if needed = 0 then acc
  else
    match !remaining_deck with
    | [] -> raise OutOfCards
    | h :: t ->
        remaining_deck := t;
        deal_helper (h :: acc) (needed - 1)

let deal = deal_helper [] cards_per_person

let draw =
  match !remaining_deck with
  | [] -> raise OutOfCards
  | h :: t ->
      remaining_deck := t;
      h
