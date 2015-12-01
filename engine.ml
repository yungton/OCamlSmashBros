open Graphics
open Character
(**attacking character should be stunned on missed attack but this brings up
   a cheating tactic, a person can keep attacking and stunning themself, thus
   achieving a constant velocity to wherever they want to go
   discuss with kevin shae shae
   *)

(* type attack moved to Character *)

type move = MLeft | MRight | MDown | MUp

let lastmove = ref (MDown,1,MDown,1)
(**These contants need to be set correctly for engine to work*)
let fallconstant = -2

let gravity = 1
let knockbackstun = 40

let aicounter = ref 0

let gravitycounter = ref 0
let momentumcounter = ref 0
let gravitycounter2 = ref 0
let momentumcounter2 = ref 0

let jumpconstant = 1

let stagetop = 103

let stageleft = 150

let stageright = 850

let newinputs = ref []

let (c1,c2) = (create Light {x=stageleft+100;y=stagetop},
                  create Medium {x=stageright-100;y=stagetop})

let ch1width = get_width c1
let ch2width = get_width c2

let stagecollision pos1 pos2 =
  let topdiff1 = pos1.y - stagetop in
  let topdiff2 = pos2.y - stagetop in
  let rightdiff1 = pos1.x - stageright in
  let rightdiff2 = pos2.x - stageright in
  let leftdiff1 = stageleft - (pos1.x + ch1width) in
  let leftdiff2 = stageleft - (pos2.x + ch2width) in
  (if topdiff1 < 0 then
    if leftdiff1 < 0 && rightdiff1 < 0 then
      if topdiff1 > leftdiff1 && topdiff1 > rightdiff1 then
      (c1.air <- false ;
      c1.jumps <- 2 ;
      change_velocity c1 {x=0;y=0} ;
      set_position c1 {x=pos1.x;y=stagetop})
      else
        if leftdiff1 > rightdiff1 then
          (change_velocity c1 {x=0;y= c1.velocity.y} ;
          set_position c1 {x=stageleft-ch1width;y=(fst (c1.hitbox)).y} )
        else
          (change_velocity c1 {x=0;y= c1.velocity.y} ;
          set_position c1 {x=stageright;y=(fst (c1.hitbox)).y} )
    else
      ()
  else
    if topdiff1 = 0 && (leftdiff1 > 0 || rightdiff1 > 0) then
      (if c1.stun > 0 then c1.jumps <- 1 else () ;
     c1.air <- true)
     else
     ()) ;
  if topdiff2 < 0 then
    if leftdiff2 < 0 && rightdiff2 < 0 then
      if topdiff2 > leftdiff2 && topdiff2 > rightdiff2 then
      (c2.air <- false ;
      c2.jumps <- 2 ;
      change_velocity c2 {x=0;y=0} ;
      set_position c2 {x=pos2.x;y=stagetop})
      else
        if leftdiff2 > rightdiff2 then
          (change_velocity c2 {x=0;y= c2.velocity.y} ;
          set_position c2 {x=stageleft-ch2width;y=(fst (c2.hitbox)).y} )
        else
          (change_velocity c2 {x=0;y= c2.velocity.y} ;
          set_position c2 {x=stageright;y=(fst (c2.hitbox)).y} )
    else
      ()
  else
    if topdiff2 = 0 && (leftdiff2 > 0 || rightdiff2 > 0) then
      (if c2.stun > 0 then c2.jumps <- 1 else () ;
     c2.air <- true)
     else
     ()

let update () =
  let newpos1test = {x=c1.velocity.x + (fst (c1.hitbox)).x;
     y=c1.velocity.y + (fst (c1.hitbox)).y} in
  let newpos2test = {x=c2.velocity.x + (fst (c2.hitbox)).x;
     y=c2.velocity.y + (fst (c2.hitbox)).y} in
  stagecollision newpos1test newpos2test ; gravitycounter := !gravitycounter + 1 ;
  gravitycounter2 := !gravitycounter2 + 1 ; momentumcounter := !momentumcounter + 1 ;
  momentumcounter2 := !momentumcounter2 + 1 ; aicounter := !aicounter + 1 ;
  let newpos1 = {x=c1.velocity.x + (fst (c1.hitbox)).x;
     y=c1.velocity.y + (fst (c1.hitbox)).y} in
  let newpos2 = {x=c2.velocity.x + (fst (c2.hitbox)).x;
     y=c2.velocity.y + (fst (c2.hitbox)).y} in
  set_position c1 newpos1 ; set_position c2 newpos2 ;
  let newvy1 = if c1.air then
                 if c1.stun < 11 then
                   if c1.velocity.y = c1.speed * fallconstant then
                     c1.speed * fallconstant
                   else
                     if !gravitycounter mod 4 = 0 then
                       max (c1.velocity.y - gravity) (c1.speed * fallconstant/2)
                     else
                       c1.velocity.y
                 else c1.velocity.y
               else 0 in
  let newvy2 = if c2.air then
                 if c2.stun < 11 then
                   if c2.velocity.y = c2.speed * fallconstant then
                     c2.speed * fallconstant
                   else
                     if !gravitycounter2 mod 4 = 0 then
                       max (c2.velocity.y - gravity) (c2.speed * fallconstant/2)
                     else
                       c2.velocity.y
                 else c2.velocity.y
               else 0 in
  let newvx1 = if c1.stun > 0 then
    ((if c1.stun - 1 = 0 then stop_attack c1 else ());
    c1.stun <- c1.stun -1 ;
    c1.velocity.x )
    else
      if !momentumcounter mod 10 = 0 then
        0
      else
      c1.velocity.x in
  let newvx2 = if c2.stun > 0 then
    ((if c2.stun - 1 = 0 then stop_attack c2 else ());
    c2.stun <- c2.stun -1 ;
    c2.velocity.x )
    else
      if !momentumcounter2 mod 10 = 0 then
        0
      else
      c2.velocity.x in
  change_velocity c1 {x=newvx1;y=newvy1} ;
  change_velocity c2 {x=newvx2;y=newvy2} ;
  let (a,b,c,d) = !lastmove in
  lastmove := (a,b-1,c,d-1)

let collide (r1: rect) (r2: rect) : bool =
  let r1p1 = fst r1 in
  let r1p2 = snd r1 in
  let r2p1 = fst r2 in
  let r2p2 = snd r2 in
  let x_collide =
    if (r1p1.x < r2p2.x && r1p1.x > r2p1.x) ||
       (r2p1.x < r1p2.x && r2p1.x > r1p1.x) ||
       (r1p1.x < r2p1.x && r1p2.x > r2p2.x) ||
       (r2p1.x < r1p1.x && r2p2.x > r1p2.x) ||
       (r1p1.x = r2p1.x) || (r1p2.x = r2p2.x) then
      true
    else false in
  let y_collide =
    if (r1p1.y < r2p2.y && r1p1.y > r2p1.y) ||
       (r2p1.y < r1p2.y && r2p1.y > r1p1.y) ||
       (r1p1.y < r2p1.y && r1p2.y > r2p2.y) ||
       (r2p1.y < r1p1.y && r2p2.y > r1p2.y) ||
       (r1p1.y = r2p1.y) || (r1p2.y = r2p2.y) then
      true
    else false in
  x_collide && y_collide

let process_attack (a: attack) (i: int) : unit =
  let (attacker, defender) = if i=0 then (c1,c2) else (c2,c1) in
  let _ = if attacker.stun > 0 then () else
  start_attack attacker a;
  match a with
  | Left ->
    (* Get a box that has width range and that is adjacent to the left of the character *)
    (let p1 = (fst attacker.hitbox) in (*Bottom left point of hitbox *)
    let p2 = (snd attacker.hitbox) in  (*Top right point of hitbox *)
    let newp1 = {x=p1.x - attacker.range;y=p1.y} in
    let newp2 = {x=p1.x;y=p2.y} in
    let attack_box = (newp1,newp2) in
    if collide attack_box defender.hitbox then (*If the attack hits*)
      (get_hit defender attack_length;
      (* This x value should be a function of dmg and attack strength *)
      change_velocity defender {x=(-1)*(defender.percent/4);
                                        y=((defender.percent/4)*35)/100};
      defender.air <- true;
      stun defender knockbackstun;
      ())
    else
      ();
    (* Attacking character is also stunned *)
    stun attacker attack_length)
  | Right ->
    (* Get a box that has width range and that is adjacent to the left of the character *)
      (let p1 = (fst attacker.hitbox) in (*Bottom left point of hitbox *)
      let p2 = (snd attacker.hitbox) in  (*Top right point of hitbox *)
      let newp1 = {x=p2.x;y=p1.y} in
      let newp2 = {x=p2.x + attacker.range;y=p2.y} in
      let attack_box = (newp1,newp2) in
      if collide attack_box defender.hitbox then (*If the attack hits*)
        (get_hit defender attack_length;
        (* This x value should be a function of dmg and attack strength *)
        change_velocity defender {x=defender.percent/4;
                                          y=((defender.percent/4)*35)/100};
        defender.air <- true;
        stun c2 knockbackstun;
        ())
      else
        ();
      (* Attacking character is also stunned *)
      stun attacker attack_length)
  | Up ->
    (* Get a box that has width range and that is adjacent to the left of the character *)
    (let p1 = (fst attacker.hitbox) in (*Bottom left point of hitbox *)
    let p2 = (snd attacker.hitbox) in  (*Top right point of hitbox *)
    let newp1 = {x=p1.x - (attacker.range/2);
                 y=p1.y + ((p2.y - p1.y)/3)} in
    let newp2 = {x=p2.x + (attacker.range/2);
                 y=p2.y + (attacker.range/2)} in
    let third = (newp2.x - newp1.x)/3 in
    let b1p1 = {x=newp1.x;y=newp1.y} in
    let b1p2 = {x=newp1.x + third;y=newp2.y} in
    let b2p1 = {x=newp1.x + third;y=newp1.y} in
    let b2p2 = {x=newp1.x + (2 * third);y=newp2.y} in
    let b3p1 = {x=newp1.x + (2 * third);y=newp1.y} in
    let b3p2 = {x=newp2.x;y=newp2.y} in
    (* Three boxes, left, middle, right *)
    let attack_box1 = (b1p1,b1p2) in
    let attack_box2 = (b2p1,b2p2) in
    let attack_box3 = (b3p1,b3p2) in
    (* Hitting only left or right boxes sends enemy at 30 degree angle.
     * Hitting left + middle or right + middle sends enemy at knockbackstun degree angle.
     * Hitting only middle or all three sends enemy straight up. *)

    (*If the attack hits all three boxes *)
    if collide attack_box1 defender.hitbox &&
       collide attack_box2 defender.hitbox &&
       collide attack_box3 defender.hitbox then
      (get_hit defender attack_length;
      (* This x value should be a function of dmg and attack strength *)
      change_velocity defender
        {x=0;
         y=defender.percent/4};
      defender.air <- true;
      stun defender knockbackstun;
      ())
    (*If the attack hits the left and middle box *)
    else if collide attack_box1 defender.hitbox &&
       collide attack_box2 defender.hitbox then
      (get_hit defender attack_length;
      (* This x value should be a function of dmg and attack strength *)
      change_velocity defender
        {x=(-1)*(defender.percent/4);
         y=((defender.percent/4)*173)/100};
      defender.air <- true;
      stun defender knockbackstun;
      ())
    (*If the attack hits the middle and right box *)
    else if collide attack_box2 defender.hitbox &&
            collide attack_box3 defender.hitbox then
      (get_hit defender attack_length;
      (* This x value should be a function of dmg and attack strength *)
      change_velocity defender
        {x=defender.percent/4;
         y=((defender.percent/4)*173)/100};
      defender.air <- true;
      stun defender knockbackstun;
      ())
    (*If the attack hits the left box only *)
    else if collide attack_box1 defender.hitbox then
      (get_hit defender attack_length;
      (* This x value should be a function of dmg and attack strength *)
      change_velocity defender
        {x=(-1)*(((defender.percent/4)*173)/100);
         y=defender.percent/4};
      defender.air <- true;
      stun defender knockbackstun;
      ())
    (*If the attack hits the right box only *)
    else if collide attack_box3 defender.hitbox then
      (get_hit defender attack_length;
      (* This x value should be a function of dmg and attack strength *)
      change_velocity defender
        {x=((defender.percent/4)*173)/100;
         y=defender.percent/4};
      defender.air <- true;
      stun defender knockbackstun;
      ())
    (*If the attack hits the middle box only *)
    else if collide attack_box2 defender.hitbox then
      (get_hit defender attack_length;
      (* This x value should be a function of dmg and attack strength *)
      change_velocity defender
        {x=0;
         y=defender.percent/4};
      defender.air <- true;
      stun defender knockbackstun;
      ())
    else
      ();
    (* Attacking character is also stunned *)
    stun attacker attack_length)
  (* Down needs to be fixed, right now it only works if character is in the air.
     A down attack on the ground should function differently, the area hit should be
     a horizontal rectangle on the bottom half of the character's hitbox.
     BOBBY - I think you should implement this same alternate way of calculating down attacks
     on the gound, to also up attacks on the ground. It's the same problem, right now
     an up attack with two people on the ground would never land because the collision
     rectangle would be above both their heads. *)
  | Down ->
    (* Get a box that has width range and that is adjacent to the left of the character *)
      (let p1 = (fst attacker.hitbox) in (*Bottom left point of hitbox *)
      let p2 = (snd attacker.hitbox) in  (*Top right point of hitbox *)
      let newp1 = {x=p1.x - (attacker.range/2);
                   y=p1.y - (attacker.range/2)} in
      let newp2 = {x=p2.x + (attacker.range/2);
                   y=p1.y + ((p2.y - p1.y)/3)} in
      let attack_box = (newp1,newp2) in
      if collide attack_box defender.hitbox then (*If the attack hits*)
        (get_hit defender attack_length;
        (* This x value should be a function of dmg and attack strength *)
        change_velocity defender {x=0;
                                          y=(-1)*(defender.percent/4)};
        stun defender knockbackstun;
        ())
      else
        ();
      (* Attacking character is also stunned *)
      stun attacker attack_length) in ()

let process_move (m: move) (i: int) : unit =
  let mover = if i=0 then c1 else c2 in
  let _ = if mover.stun > 0 then () else
  match m with
  | MLeft ->
    (let newv = {x=mover.speed * (-1);y=mover.velocity.y} in
    momentumcounter := 0 ;
    change_velocity mover newv ;
    let (a,b,c,d) = !lastmove in
    lastmove := (MLeft,12,c,d))
  | MRight ->
    (let newv = {x=mover.speed;y=mover.velocity.y} in
    momentumcounter := 0 ;
    change_velocity mover newv ;
    let (a,b,c,d) = !lastmove in
    lastmove := (MRight,12,c,d))
  | MDown ->
    (let newv = {x=mover.velocity.x;y=mover.speed * fallconstant} in
    momentumcounter := 0 ;
    change_velocity mover newv ;
    let (a,b,c,d) = !lastmove in
    lastmove := (MDown,12,c,d))
  | MUp ->
    let (a,b,c,d) = !lastmove in
    let jumpconstant = if a = MUp && b > 0 then jumpconstant*2 else jumpconstant in
    let newv = {x=mover.velocity.x;y=mover.speed * jumpconstant} in
    if mover.jumps > 0 then
      (gravitycounter := 0 ;
      change_velocity mover newv ;
      mover.jumps <- mover.jumps -1 ;
      mover.air <- true ;
      lastmove := (MUp,12,c,d))
    else
      () in ()

let airesponse () =
  if !aicounter mod 1 = 0 then
    let r = Ai.execute_response_to_state c1 c2 in
    match r with
    | "ML" -> process_move MLeft 1
    | "MD" -> process_move MDown 1
    | "MU" -> process_move MUp 1
    | "MR" -> process_move MRight 1
    | "AR" -> process_attack Right 1
    | "AU" -> process_attack Up 1
    | "AD" -> process_attack Down 1
    | "AL" -> process_attack Left 1
    | "" -> ()
    | _ -> failwith "not correct input"
  else ()


let rec tickprocessor () = (**need to call process attack*)
   (* let inputs = List.fold_right (fun x acc -> acc ^ (Char.escaped x)) !newinputs "" in *)
   let process x =
     match x with
     | 'a' -> process_move MLeft 0
     | 'w' -> process_move MUp 0
     | 's' -> process_move MDown 0
     | 'd' -> process_move MRight 0
     | 'j' -> process_attack Left 0
     | 'k' -> process_attack Down 0
     | 'l' -> process_attack Right 0
     | 'i' -> process_attack Up 0
     | _ -> () in
   let _ =
      ignore(Thread.create (fun x -> let _ = List.iter process x in update () ; airesponse ()) !newinputs) in
   newinputs := [] ;
   ignore(Thread.create (Gui.draw_characters) (c1,c2));
   Thread.delay 0.02 ;
   tickprocessor ()

let rec input_loop () =
  let newchar = read_key () in
  newinputs := !newinputs @ [newchar] ;
  input_loop ()

let start_engine () =
  let _ = Thread.create input_loop () in
  Gui.setup_window();
  Gui.draw (c1,c2);
  Thread.join (Thread.create tickprocessor ())

let _ = start_engine ()



