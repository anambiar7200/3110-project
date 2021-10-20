open Card

type table
(** Representation of dynamic gameplay.

    This module contains the functions that involve playing a card,
    checking set validity, .... **)

(*exception InvalidCombo exception NoSuchCard *)

type set_type =
  | Run
  | Group
(* The type which represents the kind of set, a Group or Run. A Group is
   a set cards with the same number, but different colors. A Run is a
   set of (increasing) consecutive numbers all in the same color. *)

type set
(** The abstract type representing a set*)

val create_set : set_type -> card list -> set
(** [create_set k cs] creates a set with kind k and cards cs. [k] is of
    type set_type and [cs] is a card list.*)

val create_table : set list -> table
(* [create_table lst] creates a table made up of the sets in lst. [lst]
   is a set list. *)

val valid_set : set -> bool
(** [valid_set s] return true if s is a valid run or group and false
    otherwise. [s] is of type set. *)

val valid_table : table -> bool
(* [valid_table tb] returns true if tb is a valid table. A table is
   valid if all the sets within it are valid sets. An empty table is
   defined as valid.*)