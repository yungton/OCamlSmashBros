open Async.Std
(** A character in OCaml Smash that is of type t. *)

type point
type rect
type attack
type attacks

type t = {
  (* Constants *)
  attacks:  attacks;
  range:    int;
  speed:    int;
  weight:   int;

  (* Variables *)
  pos:      point;
  hitbox:   rect;
  percent:  int;
  stun:     int;
  air:      bool;
  velocity: point;
  jumps:    int;
  lives:    int;
  updated:  t Async.Std.Ivar.t
}

type guy = Light | Medium | Heavy

(**
 * [create g] creates a new character that is of type g, which is of type guy.
 *)
val create : guy -> point -> t

(**
 * [attack x] simulates an attack by the character x.
 *)
val attack : t -> unit

(**
 * [stun x len] stuns updates the stun field for character x to len.
 * This means the character x becomes stunned for len seconds.
 *)
val stun : t -> int -> t

(**
 * [get_hit x dmg] adds dmg to the damage field of the character x.
 *)
val get_hit : t -> int -> t

(**
 * [change_velocity x vel] updates the velocity of the character x.
 *)
val change_velocity : t -> point -> t

(**
 * [reset x] takes a life off character x.
 *)
val reset : t -> t

val updated : t -> t Deferred.t