type point = {
  x: int;
  y: int
}
type rect = point * point
type attack = Up | Down | Left | Right

type t = {
  mutable hitbox:         rect;
  mutable percent:        int;
  mutable stun:           int;
  mutable air:            bool;
  mutable velocity:       point;
  mutable jumps:          int;
  mutable lives:          int;
  mutable current_attack: attack option;
  range:    int;
  speed:    int;
  weight:   int;
}

type guy = Light | Medium | Heavy

type attributes = {
  width:  int;
  height: int;
  speed:  int;
  weight: int;
  range:  int
}

let attributes_for_guy = function
  | Light -> {
      width  = 30;
      height = 75;
      speed  = 6;
      weight = 1;
      range  = 1
    }
  | Medium -> {
      width  = 50;
      height = 100;
      speed  = 5;
      weight = 2;
      range  = 4
    }
  | Heavy -> {
      width  = 60;
      height = 125;
      speed  = 4;
      weight = 3;
      range  = 6
    }

let attack_length = 10

let get_width c  = (snd c.hitbox).x - (fst c.hitbox).x

let get_height c = (snd c.hitbox).y - (fst c.hitbox).y

let hitbox_at_point c p =
  let width = get_width c and height = get_height c in
  (p, { x=p.x+width; y = p.y+height })

let create (g:guy) p =
  let atts = attributes_for_guy g in
  {
    hitbox = (p, {x=p.x+atts.width; y = p.y+atts.height});
    percent = 0;
    stun = 0; (* might want to have them start stunned for a 3..2..1.. thing *)
    air = false;
    velocity = {x = 0;y = 0};
    jumps = 2;
    lives = 3;
    current_attack = None;

    range = atts.range;
    speed = atts.speed;
    weight = atts.weight
  }

let set_position c p = c.hitbox <- hitbox_at_point c p

let set_jumps c i = c.jumps <- i

let start_attack c a = c.current_attack <- Some a

let stop_attack c = c.current_attack <- None

let stun c time = c.stun <- time

let get_hit c dmg = c.percent <- c.percent + dmg;
                    set_jumps c 1

let change_velocity c v = c.velocity <- v

let reset c =
  c.lives <- c.lives - 1;
  set_position c {x=475;y=400} ;
  change_velocity c {x=0;y=0} ;
  c.percent <- 0


