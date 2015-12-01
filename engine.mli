type move = MLeft | MRight | MDown | MUp

(**
 * [input_loop ()] is a recursive loop that processes inputs continuously.
 *)
val input_loop : unit -> unit

(**
 * [start_engine ()] starts the engine.
 *)
val start_engine : unit -> unit
