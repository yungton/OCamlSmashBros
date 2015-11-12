(** A character in OCaml Smash that is of type t. *)
type t

type point

type rect

type guy

(**
 * [create g] creates a new character that is of type g, which is of type guy.
 *)
val create : guy -> t

(**
 * [attack x] simulates an attack by the character x.
 *)
val attack : t -> unit

(**
 * [stun x len] stuns updates the stun field for character x to len.
 * This means the character x becomes stunned for len seconds.
 *)
val stun : t -> int -> unit

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