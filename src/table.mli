open Card

type table
(** Representation of dynamic gameplay.

    This module contains the functions that involve playing a card,
    checking set validity, .... **)

exception InvalidCombo

exception NoSuchCard

(*val check_valid : table -> bool *)

type set_type =
  | Run
  | Group
(* The type which represents the kind of set: Group, Run*)

type set
(** The abstract type representing a set*)

val valid_set : set -> bool
(** [valid_set s] return true if s is a valid run or group and false
    otherwise. [s] is of type set. *)

val create_set : set_type -> card list -> set
(** [create_set t cs] creates a set with type t and cards cs. [t] is of
    type set_type and [cs] is a card list.*)
