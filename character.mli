open Async.Std
(** A character in OCaml Smash that is of type t. *)

type point = {
  x: int;
  y: int
}
type rect = point * point
type attack = int * int
type attacks = {
  jab:    attack;
  fsmash: attack;
  usmash: attack;
  dsmash: attack;
  nspec:  attack;
  fspec:  attack;
  uspec:  attack;
  dspec:  attack
}

type t = {
  mutable hitbox:   rect;
  mutable percent:  int;
  mutable stun:     int;
  mutable air:      bool;
  mutable velocity: point;
  mutable jumps:    int;
  mutable lives:    int;
  attacks:  attacks;
  range:    int;
  speed:    int;
  weight:   int;
}

type guy = Light | Medium | Heavy

(**
 * [get_width g] returns the width of character g
 *)

val get_width : t -> int

(**
 * [create g] creates a new character that is of type g, which is of type guy.
 *)
val create : guy -> point -> t

(**
 * [moveto x p] moves the character x to point p
 *)
val moveto : t -> point -> unit

(**
 * [attack x] simulates an attack by the character x.
 *)
val attack : t -> unit

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