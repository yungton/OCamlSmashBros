open Graphics
open Character

let bg_hex = "0x0C3D6F"
let num_stars = 150
let star_size = 1
let planet_radius = 150
let planet_offset = 50
let stage_inset = 150
let stagew = 1000
let stageh = 600

let og_stage = Array.make_matrix stageh stagew white

let prev_pos = (ref {x = 0; y = 0}, ref {x = 0; y = 0})

let color_from_hex hex_string =
  let c = int_of_string hex_string in
  let r = c / 65536 and g = c / 256 mod 256 and b = c mod 256 in
  rgb r g b

let copy_matrix m1 m2 =
  for row=0 to (Array.length m1)-1 do
    for col=0 to (Array.length m1.(0))-1 do
      m1.(row).(col) <- m2.(row).(col)
    done
  done

let portion_of_og_stage x y w h =
  let result = Array.make_matrix h w white in
  for row = 0 to h-1 do
    for col = 0 to w-1 do
      result.(row).(col) <- og_stage.(row+y).(col+x)
    done;
  done;
  make_image result


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

let draw_char c f col =
  let orig_col = foreground in
  let erase_img = portion_of_og_stage (fst c.hitbox).x 
                                      (fst c.hitbox).y 
                                      (get_width c)
                                      (get_height c) in
  draw_image erase_img !(f prev_pos).x !(f prev_pos).y;
  set_color col;
  fill_rect (fst c.hitbox).x 
            (fst c.hitbox).y 
            (get_width c)
            (get_height c);
  set_color orig_col

let draw_characters (c1,c2) =
  draw_char c1 fst red;
  draw_char c2 snd blue;
  fst prev_pos := fst (c1.hitbox);
  snd prev_pos := fst (c2.hitbox)

let draw cs = 
  draw_stage_top();
  draw_characters cs;
  draw_status_box 1 red (220,10) (fst cs).percent;
  draw_status_box 2 blue ((size_x()-320),10) (snd cs).percent

let setup_window () = 
  open_graph (" " ^ (string_of_int stagew) ^ "x" ^ (string_of_int stageh)); 
  set_window_title "OCaml Smash Bros";
  draw_background();
  draw_stage();
  copy_matrix og_stage (dump_image (get_image 0 0 stagew stageh))
