open Graphics
open Character

let bg_hex = "0x0C3D6F"
let p1_hex = "0xCC3300"
let p2_hex = "0x339933"
let num_stars = 150
let star_size = 1
let planet_radius = 150
let planet_offset = 50
let stage_inset = 150
let stagew = 1000
let stageh = 600

let count = ref 0

let og_stage = Array.make_matrix stageh stagew white

let prev_pos = (ref {x=250;y=102}, ref {x=750;y=102})
let prev_attacks = (ref None, ref None)

type blast_info = {
  x :      int;
  y :      int;
  vert :   bool;
  up :     bool;
  num:     int;
  mutable frames : int;
}

let blast_length = 60

let create_blast_info x y v u n= {
  x = x;
  y = y;
  vert = v;
  up = u;
  num = n;
  frames = blast_length;
}

let blasts = ref []

let color_from_hex hex_string =
  let c = int_of_string hex_string in
  let r = c / 65536 and g = c / 256 mod 256 and b = c mod 256 in
  rgb r g b

let p1_col = color_from_hex (p1_hex)
let p2_col = color_from_hex (p2_hex)

let draw_line (x1,y1) (x2,y2) =
  let (xi,yi) = current_point() in
  moveto x1 y1;
  lineto x2 y2;
  moveto xi yi

let draw_string_with_center s (x,y) = 
  let (xi, yi) = current_point() in
  let (w,h) = text_size s in
  moveto (x-w/2) (y-h/2);
  draw_string s;
  moveto xi yi

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
  set_color black;
  set_line_width 5
  (* draw_poly poly *)


let draw_stage_base () = 
  set_color black;
  fill_rect stage_inset 0 (size_x()-2*stage_inset-1) 100

let draw_stage () = draw_stage_top(); draw_stage_base()

let draw_star () =
  let orig_col = foreground in
  let x = Random.int(size_x()) in
  let y = Random.int(size_y()) in
  set_color white;
  fill_rect x y star_size star_size;
  set_color orig_col

let draw_planet up col accent = 
  set_color col;
  let (y,a1,a2) = if up then 0-planet_offset,0,180 
                  else size_y()+planet_offset,180,360 in
  fill_arc (size_x()/2) y (size_x()) 150 a1 a2

let draw_earth () = draw_planet false blue blue
let draw_sun ()   = draw_planet true yellow yellow

let draw_background () =
  let orig_col = foreground in
  let col = color_from_hex bg_hex in 
  set_color col;
  fill_rect 0 0 (size_x()) (size_y());
  for i=0 to num_stars do draw_star() done;
  set_color orig_col

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

let bound n l u = match n with
  | x when x > u -> u
  | x when x < l -> l
  | x -> x

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

let draw_for_attack = function
  | None       -> draw_neutral_guy
  | Some Up    -> draw_up_guy
  | Some Down  -> draw_down_guy
  | Some Left  -> draw_left_guy
  | Some Right -> draw_right_guy

let draw_for_state c = draw_for_attack c.current_attack

let draw_char c f1 (f2: 'a*'a -> 'a) col =
  let orig_col = foreground in
  set_color (color_from_hex bg_hex);
  let drawf = draw_for_state c in
  let drawe = draw_for_attack !(f1 prev_attacks) in 
  draw_out_circle !(f2 prev_pos).x !(f2 prev_pos).y (get_width c) (get_height c);
  drawe !(f2 prev_pos).x !(f2 prev_pos).y (get_width c) (get_height c);
  (* let cl = if c.stun > 0 then green else col in *)
  set_color col;
  draw_out_circle (fst c.hitbox).x 
        (fst c.hitbox).y 
        (get_width c)
        (get_height c);
  drawf (fst c.hitbox).x 
        (fst c.hitbox).y 
        (get_width c)
        (get_height c);
  set_color orig_col

let form_blast_poly (heights: int list) w =
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

let rec random_list n x = if n=0 then [] else Random.int(x)::random_list (n-1) x

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


let start_blast x y vert up player =
  let info = create_blast_info x y vert up player in
  let old_blasts = !blasts in
  blasts := info :: old_blasts

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

let counter = ref 0

let start_countdown s = counter := s*60

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

let draw_characters (c1,c2) =
  incr count;
  draw_stage_top();
  (if !counter > 0 then tick_countdown false else ());
  animate_blast ();
  draw_char c1 fst fst p1_col;
  draw_char c2 snd snd p2_col;
  fst prev_pos := fst (c1.hitbox);
  snd prev_pos := fst (c2.hitbox);
  fst prev_attacks := c1.current_attack;
  snd prev_attacks := c2.current_attack;
  draw_status_box 1 p1_col (220,10) c1;
  draw_status_box 2 p2_col ((size_x()-320),10) c2;

  if !count mod 120 = 0 then (draw_background(); draw_stage()) else ()


let draw_end winner = 
  let col = foreground in
  set_color white;
  draw_string_with_center "GAME!" (stagew/2, stageh/2 + 20);
  moveto (stagew/2) (stageh/2 - 20);
  draw_string_with_center ("Player " ^ (string_of_int winner) ^ " wins!")
                          (stagew/2, stageh/2);
  moveto (stagew/2) (stageh/2 - 40);
  draw_string_with_center "Press Y for replay, N for exit" (stagew/2, stageh/2 - 20);
  set_color col

let draw (c1,c2) = 
  start_countdown 3;
  draw_background();
  draw_stage();
  draw_characters (c1,c2);
  draw_status_box 1 red (220,10) c1;
  draw_status_box 2 blue ((size_x()-320),10) c2

let setup_window () = 
  open_graph (" " ^ (string_of_int stagew) ^ "x" ^ (string_of_int stageh)); 
  set_window_title "OCaml Smash Bros";
