open Async.Std

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
  mutable pos:      point;
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

let create (g:guy) (p:point) = {
  pos = p;
  hitbox = failwith "to discuss";
  percent = 0;
  stun = 0; (* might want to have them start stunned for a 3..2..1.. thing *)
  air = false;
  velocity = {x = 0;y = 0};
  jumps = 2;
  lives = 3;

  attacks = failwith "to discuss";
  range = failwith "to discuss";
  speed = failwith "to discuss";
  weight = failwith "to discuss";
}

let moveto c p = c.pos <- p

let attack c = failwith "TODO"

let stun c time = c.stun <- time

let get_hit c dmg = c.percent <- c.percent + dmg;
                    c.jumps   <- 1

let change_velocity c v = c.velocity <- v

let reset c = c.lives <- c.lives - 1;
              c.pos   <- failwith "TODO"
