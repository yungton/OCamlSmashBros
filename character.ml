(* TODO: Discuss types of setter fuctions. In the original mli file they are all
         unit, which would mean [t] should be mutable. *)

open Async.Std

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
  uspec:  attack;
  dspec:  attack
}

type t = {
  (* Constants *)
  attacks:  attacks;
  range:    int;
  speed:    int;
  weight:   int;

  (* Variables *)
  pos:      point;
  hitbox:   rect;
  percent:  int;
  stun:     int;
  air:      bool;
  velocity: point;
  jumps:    int;
  lives:    int;
  updated:  t Ivar.t
}

type guy = Light | Medium | Heavy

let create (g:guy) (p:point) = {
  pos = p;
  hitbox = failwith "need guy size";
  percent = 0;
  stun = 0; (* might want to have them start stunned for a 3..2..1.. thing *)
  air = false;
  attacks = failwith "to discuss";
  range = failwith "to discuss";
  speed = failwith "to discuss";
  weight = failwith "to discuss";
  velocity = (0,0);
  jumps = 2;
  lives = 3; 
  updated = Ivar.create()
}

let do_update character =
  let new_character = {character with updated = Ivar.create()} in
  Ivar.fill character.updated new_character;
  new_character

let moveto c p = do_update { c with pos = p }

let attack c = failwith "TODO"

let stun c time = do_update { c with stun = time }

let get_hit c dmg = do_update { c with percent = c.percent + dmg; 
                                       jumps = 1 }

let change_velocity c v = do_update { c with velocity = v }

let reset c = do_update { c with lives = c.lives - 1;
                                 pos = failwith "TODO" }

let updated c = Ivar.read c.updated
