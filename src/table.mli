

(** Representation of dynamic gameplay.

    This module contains the functions that involve playing a card, checking
    set validity, ....
**)
type table

exception InvalidCombo

exception NoSuchCard

val check_valid : table -> bool

type set_type

type card 
(** The abstract type of values representing a card*)

type set
(** The abstract type representing a set*)


val valid_set : set -> bool
(** [valid_set s] return true if s is a valid run or group and false otherwise.
   [s] is of type set. *)