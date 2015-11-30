type attack = Left | Right | Down | Up | Neutral

type move = MLeft | MRight | MDown | MUp

(**
 * character is a list of characters in the current game.
 *)
val characters : Character.t * Character.t

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
