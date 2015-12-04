val setup_window : unit -> unit

(*Draws every character and the stage and background*)
val draw : Character.t * Character.t -> unit 

val draw_characters : Character.t * Character.t -> unit 

(** 
  * [start_blast x y vert up player] begins a blast animation with a base centered 
  * at the point (x,y). The blast if drawn vertically if [vert] is true (i.e. the
  * player dies on the top of the screen or the bottom) and is drawn horizontally
  * otherwise (i.e. the player dies on the left or right side on the screen). The
  * [player] parameter specifies the player who triggered the blast (for coloring)
  * purposes: 1 for player one and 2 for player two. If [up] is true, the blast
  * will be drawn either upwards or rightwards appropriately based on [vert].
  *
  * Example usuage:
  * [start_blast 100 0 true true 1]  -- player 1 has died on the bottom of the
  *     screen 100 pixels from the left. So we want a vertical blast facing up.
  * [start_blast 0 235 false true 2] -- player 2 has died on the left of the
  *     screen 235 pixels from the bottom. So we want a horizontal blast facing
  *     right.
  * [start_blast stage_height 400 true false 2] -- player 2 has died on the top
  *     of the screen 400 pixels from the left. So we want a vertical blast
  *     facing down.
  * [start_blast stage_width 500 false false 1] -- player 1 has died on the right
  *     of the 500 pixels from the bottom. So we want a horizontal blast facing
  *     left.
  *
  * Note: the engine only has to call this once for each blast. The animation
  * stops when the blast ends and the blast erases itself.
  * *)
val start_blast : int -> int -> bool -> bool -> int -> unit

val draw_end : int -> unit

(** Starts a countdown in SECONDS *)
val start_countdown : int -> unit