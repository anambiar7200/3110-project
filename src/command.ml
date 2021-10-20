type command_phrase = string list

type command =
  | Play of command_phrase
  | Draw
  | Stop

exception Empty

exception Malformed

let parse_input str =
  let word_list = String.split_on_char ' ' str in
  let no_space_list = List.filter (fun x -> x <> "") word_list in
  match no_space_list with
  | [] -> raise Empty
  | "play" :: t -> if t = [] then raise Malformed else Play t
  | "draw" :: t -> if t <> [] then raise Malformed else Draw
  | "stop" :: t -> if t <> [] then raise Malformed else Stop
  | _ :: t -> raise Malformed