type point = int * int
type rect = point * point
type attack = int * int
type attacks = {
  jab:    attack;
  fsmash: attack;
  usmash: attack;
  dsmash: attack;
  nspec:  attack;
  fspec:  attack;
  usped:  attack;
  dspec:  attack
}
type t = {
  pos:      point;
  hitbox:   rect;
  percent:  int;
  stun:     int;
  air:      bool;
  attacks:  attacks;
  range:    int;
  speed:    int;
  weight:   int;
  velocity: point;
  jumps:    int;
  lives:    int;
  updated:  Ivar.t
}

type guy = Light | Heavy

let create g = 

let moveto c p = { c with pos = p }

let attack c = failwith "TODO"

let stun c = failwith "TODO"

let get_hit c dmg = { c with percent = c.percent + dmg; 
                             jumps = 1 }

let change_velocity c = failwith "TODO"

let reset c = failwith "TODO"
