type attack

type move

(**
 * character is a list of characters in the current game.
 *)
val character : Character.t list

(**
 * [input_loop ()] is a recursive loop that processes inputs continuously.
 *)
val input_loop : unit -> unit

(**
 * [process_attack a i] executes an attack from player i.
 *)
val process_attack : attack -> int -> unit

(**
 * [process_move m i] executes a movement.
 *)
val process_move : move -> int -> unit

(**
 * [start_engine ()] starts the engine.
 *)
val start_engine : unit -> unit
