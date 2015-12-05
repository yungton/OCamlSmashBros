open Graphics
open Character

(* Colors for the game *)
let bg_hex = "0x0C3D6F"
let p1_hex = "0xCC3300"
let p2_hex = "0x339933"

(* Properties *)
let num_stars = 150
let star_size = 1
let planet_radius = 150
let planet_offset = 50
let stage_inset = 150
let stagew = 1000
let stageh = 600

let count = ref 0 (* A count of the number of frames that have been drawn for
                   * refresing and animating purposes *)

(* Previous states for animating purposes *)
let (pp1, pp2) = (ref {x=250;y=102}, ref {x=750;y=102}) (* previous postions *)
let (pa1, pa2) = (ref None, ref None) (* previous attacks *)

(** A type that encapsulates the needed information for drawing a blast. Frames
  * is the number of remaining frames in this blast*)
type blast_info = {
  x :      int;
  y :      int;
  vert :   bool;
  up :     bool;
  num:     int;
  mutable frames : int;
}

let blast_length = 60

(** Returns a new blast_info with the supplied data. *)
let create_blast_info x y v u n= {
  x = x;
  y = y;
  vert = v;
  up = u;
  num = n;
  frames = blast_length;
}

(* Current blasts *)
let blasts = ref []

(** [color_from_hex s] returns a color with rbg components specified in the string
  * Precondition: [s] must be of the format "0xHHHHHH" where H is any hex digit
  * (the H can vary from digit to digit)
  * *)
let color_from_hex hex_string =
  let c = int_of_string hex_string in
  let r = c / 65536 and g = c / 256 mod 256 and b = c mod 256 in
  rgb r g b

(* Colors of the players *)
let p1_col = color_from_hex (p1_hex)
let p2_col = color_from_hex (p2_hex)

(** A conveinece function for drawing a line from (x1,y1) to (x2,y2) and returning
  * then returning to the original point *)
let draw_line (x1,y1) (x2,y2) =
  let (xi,yi) = current_point() in
  moveto x1 y1;
  lineto x2 y2;
  moveto xi yi

(** A convenience function for drawing a string centered at a location instead of
  * at an origin location.  
  * *)
let draw_string_with_center s (x,y) =
  let (xi, yi) = current_point() in
  let (w,h) = text_size s in
  moveto (x-w/2) (y-h/2);
  draw_string s;
  moveto xi yi

(** Draws the polygon that is the top of the stage *)
let draw_stage_top () =
  let stage_width = size_x()-2*stage_inset in
  let inner_diff = 25 in
  let x1 = stage_inset and y1 = 100 in
  let x2 = x1 + stage_width and y2 = y1 in
  let x3 = x1 + 2*inner_diff and y3 = y1 + inner_diff in
  let x4 = x2 - 2*inner_diff and y4 = y3 in
  let poly = [|(x1,y1);(x2,y2);(x4,y4);(x3,y3)|] in
  set_color (color_from_hex "0x181818");
  fill_poly poly;
  set_color black

(* Draws the rectangle that is the base of the stage *)
let draw_stage_base () =
  set_color black;
  fill_rect stage_inset 0 (size_x()-2*stage_inset-1) 100

(* Draws both the base and top of the stage *)
let draw_stage () = draw_stage_top(); draw_stage_base()

(* Draws a star at a random location in the window *)
let draw_star () =
  let orig_col = foreground in
  let x = Random.int(size_x()) in
  let y = Random.int(size_y()) in
  set_color white;
  fill_rect x y star_size star_size;
  set_color orig_col

(* This code is for drawing planets in the background. While we ultimately choose
   not to include these effects, we decided to keep the code in case we wanted
   to include them in the future *)
(* let draw_planet up col accent =
  set_color col;
  let (y,a1,a2) = if up then 0-planet_offset,0,180
                  else size_y()+planet_offset,180,360 in
  fill_arc (size_x()/2) y (size_x()) 150 a1 a2

let draw_earth () = draw_planet false blue blue
let draw_sun ()   = draw_planet true yellow yellow *)

(** 
  * Paints the background of the window with the background color and draws stars
  * *)
let draw_background () =
  let orig_col = foreground in
  let col = color_from_hex bg_hex in
  set_color col;
  fill_rect 0 0 (size_x()) (size_y());
  for i=0 to num_stars do draw_star() done;
  set_color orig_col

(**
  * [draw_status_box pnum col (x,y) c] draws the status box for the character [c]
  * who is player number [pnum] and color [col] with an origin at the point (x,y).
  * A status box indicates the player number, the player's current percentage,
  * and the player's lives remaining.
  * *)
let draw_status_box pnum col (x,y) c =
  let orig_col = foreground in
  let (xi, xy) = current_point() in
  set_color col;
  fill_rect x y 100 75;
  set_color white;
  set_text_size 100;
  moveto (x+25) (y+55);
  draw_string ("Player " ^ (string_of_int pnum));
  moveto (x+30) (y+30);
  draw_string ((string_of_int c.percent) ^ "%");
  moveto (x+30) (y+10);
  draw_string ("Lives: " ^ (string_of_int c.lives));
  moveto xi xy;
  set_color orig_col

(** [bound n lower upper] is [n] if n is between lower and upper, is [lower] if [n] 
  * is less than [lower], and is [upper] if [n] is greater than [upper].
  * In other words, the function bounds a number [n] between [lower] and [upper]
  * *)
let bound n l u = match n with
  | x when x > u -> u
  | x when x < l -> l
  | x -> x

(** Draws a circle on the edge of the window that indicates where a player is
  * offscreen. *)
let draw_out_circle x y w h =
  let circle_radius = 10 in
  let circle_inset  = 20 in
  if x + w < 0 then
  draw_circle circle_inset (bound y circle_inset (stageh-circle_inset)) circle_radius
  else if x > stagew then
  draw_circle (stagew - circle_inset) (bound y circle_inset (stageh-circle_inset)) circle_radius
  else if y + h < 0 then
  draw_circle (bound x circle_inset (stagew-circle_inset)) circle_inset circle_radius
  else if y > stageh then
  draw_circle (bound x circle_inset (stagew-circle_inset)) (stageh - circle_inset) circle_radius

(* For each draw_x_guy function, the parameters are the same as those in fill_rect *)

(* Draws a character in the neutral pose *)
let draw_neutral_guy x y w h =
  let xb = x+w/2 in
  let yab = y+h-w and ylb = y+(min w (h/4)) in
  (* Head *)
  fill_circle xb (y+h-w/2) (w/2);

  set_line_width 5;

  (* Body *)
  draw_line (xb, y+h-5) (xb, ylb);

  (* Arms *)
  draw_line (xb, yab) (x, yab-w/2);
  draw_line (xb, yab) (x+w, yab-w/2);

  (* Legs *)
  draw_line (xb, ylb) (x, y);
  draw_line (xb, ylb) (x+w, y)

(* Draws a character in the rightward attacking pose *)
let draw_right_guy x y w h =
  let xb = x+w/2 in
  let yab = y+h-w and ylb = y+(min w (h/4)) in
  (* Head *)
  fill_circle xb (y+h-w/2) (w/2);

  set_line_width 5;

  (* Body *)
  draw_line (xb, y+h-5) (xb, ylb);

  (* Arms *)
  draw_line (xb, yab) (x+w, yab-10);
  draw_line (xb, yab) (x+w, yab+5);

  (* Legs *)
  draw_line (xb, ylb) (x, y);
  draw_line (xb, ylb) (x+w, y)

(* Draws a character in the leftward attacking pose *)
let draw_left_guy x y w h =
  let xb = x+w/2 in
  let yab = y+h-w and ylb = y+(min w (h/4)) in
  (* Head *)
  fill_circle xb (y+h-w/2) (w/2);

  set_line_width 5;

  (* Body *)
  draw_line (xb, y+h-5) (xb, ylb);

  (* Arms *)
  draw_line (xb, yab) (x, yab-10);
  draw_line (xb, yab) (x, yab+5);

  (* Legs *)
  draw_line (xb, ylb) (x, y);
  draw_line (xb, ylb) (x+w, y)

(* Draws a character in the upward attacking pose *)
let draw_up_guy x y w h =
  let xb = x+w/2 in
  let yab = y+h-w and ylb = y+(min w (h/4)) in
  (* Head *)
  fill_circle xb (y+h-w/2) (w/2);

  set_line_width 5;

  (* Body *)
  draw_line (xb, y+h-5) (xb, ylb);

  (* Arms *)
  draw_line (xb, yab) (x, yab+w/2);
  draw_line (xb, yab) (x+w, yab+w/2);

  (* Legs *)
  draw_line (xb, ylb) (x, y);
  draw_line (xb, ylb) (x+w, y)

(* Draws a character in the downward attacking pose *)
let draw_down_guy x y w h =
  let xb = x+w/2 in
  let yab = y+h-w and ylb = y+(min w (h/4)) in
  (* Head *)
  fill_circle xb (y+h-w/2) (w/2);

  set_line_width 5;

  (* Body *)
  draw_line (xb, y+h-5) (xb, ylb);

  (* Arms *)
  draw_line (xb, yab) (x+10, yab-w/2);
  draw_line (xb, yab) (x+w-10, yab-w/2);

  (* Legs *)
  draw_line (xb, ylb) (x+10, y);
  draw_line (xb, ylb) (x+w-10, y)

(* Returns the appropriate drawing function for a given attack *)
let draw_for_attack = function
  | None       -> draw_neutral_guy
  | Some Up    -> draw_up_guy
  | Some Down  -> draw_down_guy
  | Some Left  -> draw_left_guy
  | Some Right -> draw_right_guy

(* Returns the appropriate drawing function for a given character *)
let draw_for_state c = draw_for_attack c.current_attack

(** [draw_char c i col] draws a character of state [c] with color [col].
  * [i] indicates which player should be drawn. Supply 1 for player one and 2
  * for player 2. *)
let draw_char c i col =
  let orig_col = foreground in
  set_color (color_from_hex bg_hex);
  let (pp, pa) = if i=1 then (pp1, pa1) else (pp2, pa2) in
  let drawf = draw_for_state c in
  let drawe = draw_for_attack !pa in
  draw_out_circle !pp.x !pp.y (get_width c) (get_height c);
  drawe !pp.x !pp.y (get_width c) (get_height c);
  set_color col;
  draw_out_circle (fst c.hitbox).x
        (fst c.hitbox).y
        (get_width c)
        (get_height c);
  drawf (fst c.hitbox).x
        (fst c.hitbox).y
        (get_width c)
        (get_height c);
  pp := fst (c.hitbox);
  pa := c.current_attack;
  set_color orig_col

(* These functions are for manipulating polygons for blasts that aren't just
   rectangles. The polygons didn't look very good, so we chose to go with
   rectangles, but it's possible that we could want to change that in the future
   so we felt it was best to keep the following code commented out instead of
   deleting it. *)
(* let form_blast_poly (heights: int list) w =
  let dx = w / (List.length heights) in
  let rec assign_x_to_height dx n = function
    | []   -> []
    | h::t -> (n*dx, h)::assign_x_to_height dx (n+1) t in
  Array.of_list ((w,0)::(0,0)::
                 (assign_x_to_height dx 0 heights @ [(w, List.hd heights)]))

let rotate_poly_about_origin poly = Array.map (fun (x,y) -> (-y, x)) poly

let rec rotate_n_times n poly =
  if n = 0 then poly else rotate_n_times (n-1) (rotate_poly_about_origin poly)

let shift_poly poly (dx, dy) =
  Array.map (fun (x,y) -> (x+dx, y+dy)) poly

let rec random_list n x = if n=0 then [] else Random.int(x)::random_list (n-1) x *)

(** Draws a blast with a base centered at (x,y).
  * [vertical] = "the blast should be drawn tall, not wide"
  * [up]       = "the blast should be drawn up if veritcal and left otherwise"*)
let draw_blast info erase =
  let min_width  = 30  and var_width  = 70 in
  let min_height = 200 and var_height = 70 in
  let width  = min_width  + Random.int(var_width) in
  let height = min_height + Random.int(var_height) in

  let max_width = min_width + var_width in
  let max_height = min_height + var_height in

  let (w, mw)  = if info.vert then (width, max_width) else (height, max_height) in
  let (h, mh)  = if info.vert then (height, max_height) else (width, max_width) in
  let (xr, mxr) = match info.vert, info.up with
  | true, true -> (info.x - w/2, info.x - mw/2)
  | true, false -> (info.x - w/2, info.x - mw/2)
  | false, true -> (info.x, info.x)
  | false, false -> (info.x - w, info.x - mw) in

  let (yr, myr) = match info.vert, info.up with
  | true, true -> (info.y, info.y)
  | true, false -> (info.y - h, info.y - mh)
  | false, true -> (info.y - h/2, info.y - mh/2)
  | false, false -> (info.y - h/2, info.y - mh/2) in

  let col = foreground in
  set_color (color_from_hex bg_hex);
  fill_rect mxr myr mw mh;
  let player_col = if info.num = 1 then p1_col else p2_col in
  set_color player_col;
  (if erase then () else fill_rect xr yr w h);
  set_color col

(* see .mli
 * Adds a new blast to the list of current blasts *)
let start_blast x y vert up player =
  let info = create_blast_info x y vert up player in
  let old_blasts = !blasts in
  blasts := info :: old_blasts

(** Creates a new randomly sized blast for each blast in the current blasts list
  * then decreases the remaining frames for each one. Removes a blast from the 
  * list and erases it if this was the last frame for the blast. *)
let animate_blast () =
  let rec helper acc = function
  | [] -> acc
  | b::bs ->
    draw_blast b false;
    b.frames <- b.frames - 1;
    let new_acc = if b.frames = 0 then (draw_blast b true; acc) else b::acc in
    helper new_acc bs
  in

  blasts := helper [] !blasts

(* A counter for a countdown *)
let counter = ref 0

(* see .mli *)
let start_countdown s = counter := s*60

(** Draws a string corresponding to the number of seconds remaining on the countdown
  * in the center of the screen. [erase] indicates if the function should just
  * erase the string and not draw a new one*)
let tick_countdown erase =
  let col = foreground in
  set_color (color_from_hex bg_hex);
  fill_rect (stagew/2 - 15) (stageh/2 - 15) 30 30;
  set_color yellow;
  (if erase then ()
    else
      let s = if (!counter > 1)
              then string_of_int ((!counter/60) + 1)
              else "GO!" in
      draw_string_with_center s (stagew/2, stageh/2));
  counter := !counter - 1;
  set_color col

(* see .mli *)
let draw_end winner =
  let winner = if winner = 1 then 2 else 1 in
  let col = foreground in
  blasts := [];
  set_color white;
  draw_string_with_center "GAME!" (stagew/2, stageh/2 + 20);
  moveto (stagew/2) (stageh/2 - 20);
  draw_string_with_center ("Player " ^ (string_of_int winner) ^ " wins!")
                          (stagew/2, stageh/2);
  moveto (stagew/2) (stageh/2 - 40);
  draw_string_with_center "Press Y for replay, N for exit" (stagew/2, stageh/2 - 20);
  set_color col

(* see .mli *)
let draw (c1,c2) =
  incr count;
  draw_stage_top();
  (if !counter > 0 then tick_countdown false else ());
  animate_blast ();
  draw_char c1 1 p1_col;
  draw_char c2 2 p2_col;
  draw_status_box 1 p1_col (220,10) c1;
  draw_status_box 2 p2_col ((size_x()-320),10) c2;

  if !count mod 120 = 0 then (draw_background(); draw_stage()) else ()

(* see .mli *)
let setup_window () =
  open_graph (" " ^ (string_of_int stagew) ^ "x" ^ (string_of_int stageh));
  set_window_title "OCaml Smash Bros";
  draw_background();
  draw_stage()
