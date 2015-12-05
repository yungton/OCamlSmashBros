(** A character in OCaml Smash that is of type t. *)

type point = {
  x: int;
  y: int
}
type rect = point * point
type attack = Up | Down | Left | Right

type t = {
  mutable hitbox:         rect;
  mutable percent:        int;
  mutable stun:           int;
  mutable air:            bool;
  mutable velocity:       point;
  mutable jumps:          int;
  mutable lives:          int;
  mutable current_attack: attack option;
  range:    int;
  speed:    int;
  weight:   int;
}

type guy = Light | Medium | Heavy

val attack_length : int

(**
 * [get_width g] returns the width of character g
 *)

val get_width : t -> int

(**
 * [get_height g] returns the width of character g
 *)

val get_height : t -> int

(**
 * [create g] creates a new character that is of type g, which is of type guy.
 *)
val create : guy -> point -> t

(**
 * [moveto x p] moves the character x to point p
 *)
val set_position : t -> point -> unit

(**
 * [attack x] simulates an attack by the character x.
 *)
val start_attack : t -> attack -> unit

val stop_attack : t -> unit

(**
 * [stun x len] stuns updates the stun field for character x to len.
 * This means the character x becomes stunned for len seconds.
 *)
val stun : t -> int -> unit

val set_jumps : t -> int -> unit

(**
 * [get_hit x dmg] adds dmg to the damage field of the character x.
 *)
val get_hit : t -> int -> unit

(**
 * [change_velocity x vel] updates the velocity of the character x.
 *)
val change_velocity : t -> point -> unit

(**
 * [reset x] takes a life off character x.
 *)
val reset : t -> unit