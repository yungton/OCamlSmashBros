type point = {
  x: int;
  y: int
}
type rect = point * point
type attack = int * int

type attacks = {
  jab:    attack;
  fsmash: attack;
  usmash: attack;
  dsmash: attack;
  nspec:  attack;
  fspec:  attack;
  uspec:  attack;
  dspec:  attack
}

type t = {
  mutable hitbox:   rect;
  mutable percent:  int;
  mutable stun:     int;
  mutable air:      bool;
  mutable velocity: point;
  mutable jumps:    int;
  mutable lives:    int;
  attacks:  attacks;
  range:    int;
  speed:    int;
  weight:   int;
}

type guy = Light | Medium | Heavy

let get_width c  = (snd c.hitbox).x - (fst c.hitbox).x

let get_height c = (snd c.hitbox).y - (fst c.hitbox).y

let hitbox_at_point c p = 
  let width = get_width c and height = get_height c in
  (p, { x=p.x+width; y = p.y+height })

let test_attack = (1,1)

let test_attacks = {
  jab    = test_attack;
  fsmash = test_attack;
  usmash = test_attack;
  dsmash = test_attack;
  nspec  = test_attack;
  fspec  = test_attack;
  uspec  = test_attack;
  dspec  = test_attack
}
let create (g:guy) p = {
  hitbox = (p, {x=p.x+50; y = p.y+100}); (* actual dimensions change with guy*)
  percent = 0;
  stun = 0; (* might want to have them start stunned for a 3..2..1.. thing *)
  air = false;
  velocity = {x = 0;y = 0};
  jumps = 2;
  lives = 3;

  attacks = test_attacks;
  range = 1;
  speed = 1;
  weight = 1
}

let set_position c p = c.hitbox <- hitbox_at_point c p

let set_jumps c i = c.jumps <- i

let attack c = failwith "attack TODO"

let stun c time = c.stun <- time

let get_hit c dmg = c.percent <- c.percent + dmg;
                    set_jumps c 1

let change_velocity c v = c.velocity <- v

let reset c = c.lives <- c.lives - 1; (* need to change hitbox too*)
