(** A character in OCaml Smash that is of type t. *)
type t

type point

type rect

(**
 * [attack x] simulates an attack by the character x.
 *)
val attack : t -> t

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
 * [update x] creates a new record and fills in the old Ivar with
 * the new character.
 *)
val update : t -> t Deferred.t