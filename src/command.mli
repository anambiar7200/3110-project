type command_phrase = string list
(** The type of [command_phrase] represents the command phrase that can
    be part of a player's command. Each element of the list represents a
    word of command phrase. Each string element in the list is defined
    as a consecutive sequence of non-space alphanumeric characters. No
    elements of the list contain any leading, interal or trailing
    spaces. The list is in the same order as the words in the original
    player command.

    - If player command is
      ["play group 1 black 0 1 blue 26 1 orange 52"], then the command
      phrase is
      [\[ "group"; "1"; "black"; "0"; "1"; "blue"; "26"; "1"; "orange"; "52"\]]
    - If player command is
      ["play  run 10 blue 35 11 blue 36  12 blue 37"], then the command
      phrase is
      [\["run"; "10"; "blue"; "35"; "11"; "blue"; "36"; "12"; "blue"; "37"\]]
      A command_phrase is not allowed to be the empty list. *)

(** Play: A player submit one group or one run with the verb play
    followed by run or group, number, color, index of cards. Draw: A
    player draws a card. Draw takes no object. Stop: A player exits the
    game engine with this verb. Stop takes no object. EndTurn: A player
    can signal that his/her turn has ended. Game moves on to the next
    player. EndTurn takes no object. Commands are case sensitive *)

type command =
  | Play of command_phrase
  | Edit of command_phrase
  | Draw
  | Stop
  | EndTurn

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is encountered. *)

val parse_input : string -> command
(** [parse_input str] parses a players's input into a [command]. The
    first word (i.e., consecutive sequence of non-space characters) of
    [str] becomes the verb. The rest of the words are command phrase.
    Examples:
    [parse_input "  play group 1 black 0 1 blue 26 1 orange 52 "] is
    [Play \[ "group"; "1"; "black"; "0"; "1"; "blue"; "26"; 
        "1"; "orange"; "52"\]]
    [parse_input "play  run 10 blue 35 11 blue 36 12 blue 37"] is
    [Play \["run"; "10"; "blue"; "35"; "11"; "blue"; "36"; "12"; "blue"; "37"\]]
    [parse_input "stop"] is [Stop] [parse_input "draw"] is [Draw]
    [parse_input "endturn"] is [EndTurn]

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters.

    Raises: [Empty] if [str] is the empty string or contains only
    spaces.

    Raises: [Malformed] if the command is malformed. A command is
    {i malformed} if the verb is neither "play", "stop", "draw" nor
    endturn if the verb is "stop", "draw", or "endturn" and there is a
    non-empty command phrase, or if the verb is "play" and there is an
    empty command phrase. *)
