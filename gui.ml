open Graphics

let bg_hex = "0x0C3D6F"
let num_stars = 150
let star_size = 1
let planet_radius = 150
let planet_offset = 50
let stage_inset = 150

let color_from_hex hex_string =
  let c = int_of_string hex_string in
  let r = c / 65536 and g = c / 256 mod 256 and b = c mod 256 in
  rgb r g b

let draw_stage_top () = 
  let stage_width = size_x()-2*stage_inset in
  let inner_diff = 50 in 
  let x1 = stage_inset and y1 = 100 in
  let x2 = x1 + stage_width and y2 = y1 in
  let x3 = x1 + inner_diff and y3 = y1 + inner_diff in
  let x4 = x2 - inner_diff and y4 = y3 in
  let poly = [|(x1,y1);(x2,y2);(x4,y4);(x3,y3)|] in 
  set_color (color_from_hex "0x222222");
  fill_poly poly;
  set_color black;
  set_line_width 5;
  draw_poly poly


let draw_stage_base () = 
  set_color black;
  fill_rect stage_inset 0 (size_x()-2*stage_inset-1) 100

let draw_stage () = fill_rect stage_inset 100 (size_x()-2*stage_inset) 50

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
  (* draw_planet false yellow yellow; *)
  (* draw_earth(); *)
  (* draw_sun(); *)
  set_color orig_col

let draw_status_box pnum col (x,y) percent = 
  let orig_col = foreground in
  let (xi, xy) = current_point() in
  set_color col;
  fill_rect x y 100 75;
  set_color white;
  set_text_size 100;
  moveto (x+25) (y+55);
  draw_string ("Player " ^ (string_of_int pnum));
  moveto (x+30) (y+30);
  draw_string ((string_of_int percent) ^ "%");
  moveto xi xy;
  set_color orig_col
  

let draw_char col (x,y) w h =
  let orig_col = foreground in
  set_color col;
  fill_rect x y w h;
  set_color orig_col

let _ = 
  open_graph " 1000x600"; 
  set_window_title "OCaml Smash Bros";
  draw_background();
  draw_stage_top();
  draw_stage_base();
  draw_char red (300,125) 25 75;
  draw_char blue (700,125) 25 75;
  draw_status_box 1 red (220,10) 69;
  draw_status_box 2 blue ((size_x()-320),10) 69

let rec loop () = loop()

let _ = loop()
