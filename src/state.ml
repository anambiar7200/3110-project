open Card
open Player
open Drawing
open Table
open Command
open Add

exception Spaceholder

type state = {
  current_deck : card list;
  current_table : set list list;
  current_player : player;
  next_player : player;
  first_plays : int list;
  play_count : int;
}

(*shuffled card_deck*)
let shuffled_card_deck = Drawing.drawing_init ()

(*14 cards dealing to a player, plus the remaining card deck*)
let dealed_card, remain_card_deck = deal shuffled_card_deck

let dealed_card2, remain_card_deck2 = deal remain_card_deck

let rec zeros n = if n = 0 then [] else 0 :: zeros (n - 1)

let create_state
    (deck : card list)
    (tb : set list list)
    (cp : player)
    (np : player)
    (fp : int list)
    (pc : int) =
  {
    current_deck = deck;
    current_table = tb;
    current_player = cp;
    next_player = np;
    first_plays = fp;
    play_count = pc;
  }

let init_state : state =
  {
    current_deck = remain_card_deck2;
    current_table = [];
    current_player = build_player dealed_card;
    next_player = build_player dealed_card2;
    first_plays = [ 0; 0 ];
    play_count = 0;
  }

let card_sum (cards : card list) =
  List.fold_left (fun acc x -> acc + Card.get_number x) 0 cards

let current_deck_lst (st : state) = st.current_deck

let current_player_hand (st : state) = st.current_player

let current_table_lst (st : state) = st.current_table

let current_next_player (st : state) = st.next_player

let first_pl (st : state) = st.first_plays

let current_count (st : state) = st.play_count

type result =
  | Legal of state
  | Illegal
  | LegalStop
  | LegalSwitch of state

let first_play st =
  let player = st.play_count mod 2 in
  List.nth st.first_plays player = 1

(**[draw_state st] is the new state after [st.current_player] draws a
   card from [st.current_deck]. The new [current_deck] will be the
   remaining deck from drawing. The table stays the same. The new
   [current_player] will have [st.current_player] with the card drawn
   added to it.*)
let draw_state (st : state) =
  let card_drawn, remain_deck = draw st.current_deck in
  {
    current_deck = remain_deck;
    current_table = st.current_table;
    current_player = st.next_player;
    next_player = Player.add_to_player st.current_player card_drawn;
    first_plays = st.first_plays;
    play_count = succ st.play_count;
  }

(**[play_mul_card] is the player hand after playing multiple cards.
   These cards from an input card list will not present in the new
   player hand. This function calls [play_Card] in module player*)
let rec play_mul_card (clst : card list) (p : player) =
  match clst with
  | [] -> p
  | h :: t -> play_mul_card t (Player.play_card2 (get_index h) p)

(**[command_phr_translation] returns a tuple of the kind of a set with a
   card list based on the command phrase*)
let command_phr_translation (str : string list) =
  match str with
  | [] -> raise Malformed
  | h :: t -> (h, t)

let edit_phr_translation (str : string list) =
  match str with
  | [] -> raise Malformed
  | [ p; r; c; crd ] ->
      ( p,
        int_of_string r,
        int_of_string c,
        List.nth card_deck2 (int_of_string crd) )
  | something_else -> raise Malformed

(**[match_color] is the color type of the card. The function matches a
   string representing a color to a [color_type] in modele card*)
let match_color (str : string) =
  match str with
  | "black" -> Black
  | "orange" -> Orange
  | "red" -> Red
  | "blue" -> Blue
  | something -> raise Malformed

let update_first_play st =
  let pl = st.play_count mod 2 in
  match st.first_plays with
  | [] -> raise Spaceholder
  | [ h; t ] -> if pl = 0 then [ h + 1; t ] else [ h; t + 1 ]
  | something -> raise Spaceholder

(**[match_phrase_helper] matches a list of string representing card
   information to a card list. It calls [buld_card] in module card, such
   that we can call [play_mul_card] later on that card*)
let rec match_phrase_helper (str : string list) =
  match str with
  | [] -> []
  | h :: m :: e :: t ->
      List.nth card_deck (int_of_string e) :: match_phrase_helper t
  | something_else -> raise Malformed

(**[match_phrase] matches a list of string representing card information
   to a card list. It raises a Command.Malformed exception if the
   command is an empty list and calls [match_phrase_helper] if the
   command is not an empty list*)
let rec match_phrase (str : string list) =
  match str with
  | [] -> raise Command.Malformed
  | _ -> match_phrase_helper str

(**[match_set_type] matches a valid to its corresponding set_type in
   module table*)
let match_set_type (str : string) =
  match str with
  | "group" -> Group
  | "run" -> Run
  | something_else -> raise Malformed

(**[play_state st] is the new state after [st.current_player] plays some
   cards according to a command. The [current_deck] will stay the same.
   The table will have the new set added. The new [current_player] will
   have [st.current_player] with the cards played removed from it.*)
let play_state (st : state) ((str1, str2) : string * string list) =
  let set = create_set (match_set_type str1) (match_phrase str2) in
  let valid_set = Table.valid_set set in
  if
    (not (first_play st))
    && (card_sum (get_cards set) < 20 || not valid_set)
  then raise InvalidCombo
  else if valid_set then
    let card_lst = match_phrase str2 in
    {
      current_deck = st.current_deck;
      current_table =
        new_table
          (add_set
             (create_set (match_set_type str1) card_lst)
             st.current_table);
      current_player = st.next_player;
      next_player = play_mul_card card_lst st.current_player;
      first_plays = st.first_plays;
      play_count = st.play_count;
    }
  else raise InvalidCombo

(**[switch_state] switches the current player hand to the next player
   hand, and make the next player hand to be the current player hand*)
let switch_state (st : state) =
  {
    current_deck = st.current_deck;
    current_table = st.current_table;
    current_player = st.next_player;
    next_player = st.current_player;
    first_plays = st.first_plays;
    play_count = succ st.play_count;
  }

(*for parameter [c]index starts from 0 for the table - user c : 0-> 0 in
  actual table index - user c : 1 -> 2 in actual table index - user c :
  c -> c in actual table index*)

(**[edit_state] is called when the player wants to either prepend or
   append a card to a set. The function will return a new state with a
   new table. However, [edit_state] is allowed to be called multiple
   times, until the player calles [EndTurn] to switch player*)
let edit_state
    (st : state)
    ((str, r, c, cd) : string * int * int * card) =
  let cur_tb = current_table_lst st in
  if r > List.length cur_tb || c * 2 > List.length (List.nth cur_tb r)
  then raise InvalidCombo
  else
    let s = edit_helper str cd (List.nth (List.nth cur_tb r) (c * 2)) in
    let valid_set = valid_set s in
    if
      (not (first_play st))
      && (card_sum (get_cards s) < 20 || not valid_set)
    then raise InvalidCombo
    else if valid_set then
      let new_tb = replace cur_tb r (c * 2) s in
      {
        current_deck = st.current_deck;
        current_table = new_tb;
        current_player = play_card2 (get_index cd) st.current_player;
        next_player = st.next_player;
        first_plays = update_first_play st;
        play_count = st.play_count;
      }
    else raise InvalidCombo

(*If the player decides to play, call play_state. If the player decides
  to draw, call draw_state*)
let go (c : command) (st : state) =
  try
    match c with
    | Play str -> Legal (play_state st (command_phr_translation str))
    | Edit str -> Legal (edit_state st (edit_phr_translation str))
    | Draw -> Legal (draw_state st)
    | Stop -> LegalStop
    | EndTurn -> LegalSwitch (switch_state st)
  with
  | Spaceholder -> Illegal
  | InvalidCombo -> Illegal
  | NoSuchCard -> Illegal
  | OutOfCards -> Illegal
  (* | Drawing.OutOfCards -> Illegal *)
  (* | Player.OutOfCards -> Illegal *)
  | NotYourCard -> Illegal