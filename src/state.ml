open Card
open Player
open Drawing
open Table
open Command
open Graph

type state = {
  current_deck : card list;
  current_table : set list list;
  current_player : player;
  next_player : player;
}

(*shuffled card_deck*)
let shuffled_card_deck = Drawing.drawing_init ()

(*14 cards dealing to a player, plus the remaining card deck*)
let dealed_card, remain_card_deck = deal shuffled_card_deck

let dealed_card2, remain_card_deck2 = deal remain_card_deck

let create_state
    (deck : card list)
    (tb : set list list)
    (cp : player)
    (np : player) =
  {
    current_deck = deck;
    current_table = tb;
    current_player = cp;
    next_player = np;
  }

let init_state : state =
  {
    current_deck = remain_card_deck2;
    current_table = [];
    current_player = build_player dealed_card;
    next_player = build_player dealed_card2;
  }

let current_deck_lst (st : state) = st.current_deck

let current_player_hand (st : state) = st.current_player

let current_table_lst (st : state) = st.current_table

let current_next_player (st : state) = st.next_player

type result =
  | Legal of state
  | Illegal
  | LegalStop
  | LegalSwitch of state

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
    current_player = Player.add_to_player st.current_player card_drawn;
    next_player = st.next_player;
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
  | [] -> raise Command.Malformed
  | h :: t -> (h, t)

(**[match_color] is the color type of the card. The function matches a
   string representing a color to a [color_type] in modele card*)
let match_color (str : string) =
  match str with
  | "black" -> Black
  | "orange" -> Orange
  | "red" -> Red
  | "blue" -> Blue
  | something -> raise Malformed

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
  if
    Table.valid_set
      (create_set (match_set_type str1) (match_phrase str2))
  then
    let card_lst = match_phrase str2 in
    {
      current_deck = st.current_deck;
      current_table =
        Graph.new_table
          (Graph.add_set
             (create_set (match_set_type str1) card_lst)
             st.current_table);
      current_player = play_mul_card card_lst st.current_player;
      next_player = st.next_player;
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
  }

(*If the player decides to play, call play_state. If the player decides
  to draw, call draw_state*)
let go (c : command) (st : state) =
  try
    match c with
    | Command.Play str ->
        Legal (play_state st (command_phr_translation str))
    | Command.Draw -> Legal (draw_state st)
    | Command.Stop -> LegalStop
    | Command.EndTurn -> LegalSwitch (switch_state st)
  with
  | Table.InvalidCombo -> Illegal
  | Table.NoSuchCard -> Illegal
  | Drawing.OutOfCards -> Illegal
  | Player.OutOfCards -> Illegal
  | Player.NotYourCard -> Illegal
