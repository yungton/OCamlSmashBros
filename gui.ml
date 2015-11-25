open Graphics

let draw_status_box col (x,y) percent = 
  let orig_col = foreground in
  let (xi, xy) = current_point() in
  set_color col;
  fill_rect x y 100 75;
  set_color white;
  moveto (x+20) (y+40);
  set_text_size 100;
  draw_string ((string_of_int percent) ^ "%");
  moveto xi xy;
  set_color orig_col
  

let draw_char col (x,y) w h =
  let orig_col = foreground in
  set_color col;
  fill_rect x y w h;
  set_color orig_col

let _ = 
  open_graph " 800x500"; 
  set_window_title "OCaml Smash Bros";
  draw_char red (300,150) 25 75;
  draw_char blue (500,150) 25 75;
  fill_rect 100 100 (size_x()-200) 50;
  draw_status_box red (120,10) 69;
  draw_status_box blue ((size_x()-220),10) 69

let rec loop() = loop()

let _ = loop()
