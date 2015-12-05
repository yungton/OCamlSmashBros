open Graphics
open Character

()

type move = MLeft | MRight | MDown | MUp

let lastmove = ref (MDown,1,MDown,1)
(**These contants need to be set correctly for engine to work*)
let fallconstant = -2

let gravity = 1
let knockbackstun = 40

(**These counters let us control for how many frames certain effects last when
   we mod them.*)

let aicounter = ref 0

let gravitycounter = ref 0
let momentumcounter = ref 0
let gravitycounter2 = ref 0
let momentumcounter2 = ref 0

let jumpconstant = 1

let stagetop = 103

let stageleft = 150

let stageright = 850

let continue = ref true

let newinputs = ref []

let _ = Random.self_init ()

(** [chargen ()] generates a pair of random character types.*)
let chargen () =
  match (Random.int 3,Random.int 3) with
  | (0,0) -> (Light, Light)
  | (0,1) -> (Light, Medium)
  | (0,2) -> (Light, Heavy)
  | (1,0) -> (Medium, Light)
  | (1,1) -> (Medium, Medium)
  | (1,2) -> (Medium, Heavy)
  | (2,0) -> (Heavy, Light)
  | (2,1) -> (Heavy, Medium)
  | (2,2) -> (Heavy, Heavy)
  | _ -> failwith "random doesnt work"

let (rand1,rand2) = chargen ()

let c1 = ref (create rand1 {x=stageleft+100;y=stagetop})

let c2 = ref (create rand2 {x=stageright-100;y=stagetop})

let ch1width = get_width (!c1)
let ch2width = get_width (!c2)

(**[stagecollision pos1 pos2] calculates if the characters at positions pos1 and pos2
 hit the stage and processes the changes that need to made to the characters as
a result. It returns unit. Positions are of type point from character.*)
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
      ((!c1).air <- false ;
      (!c1).jumps <- 2 ;
      change_velocity (!c1) {x=0;y=0} ;
      set_position (!c1) {x=pos1.x;y=stagetop})
      else
        if leftdiff1 > rightdiff1 then
          (change_velocity (!c1) {x=0;y= (!c1).velocity.y} ;
          set_position (!c1) {x=stageleft-ch1width;y=(fst ((!c1).hitbox)).y} )
        else
          (change_velocity (!c1) {x=0;y= (!c1).velocity.y} ;
          set_position (!c1) {x=stageright;y=(fst ((!c1).hitbox)).y} )
    else
      ()
  else
    if topdiff1 = 0 && (leftdiff1 > 0 || rightdiff1 > 0) then
      (if (!c1).stun > 0 then (!c1).jumps <- 1 else () ;
     (!c1).air <- true)
     else
     ()) ;
  if topdiff2 < 0 then
    if leftdiff2 < 0 && rightdiff2 < 0 then
      if topdiff2 > leftdiff2 && topdiff2 > rightdiff2 then
      ((!c2).air <- false ;
      (!c2).jumps <- 2 ;
      change_velocity (!c2) {x=0;y=0} ;
      set_position (!c2) {x=pos2.x;y=stagetop})
      else
        if leftdiff2 > rightdiff2 then
          (change_velocity (!c2) {x=0;y= (!c2).velocity.y} ;
          set_position (!c2) {x=stageleft-ch2width;y=(fst ((!c2).hitbox)).y} )
        else
          (change_velocity (!c2) {x=0;y= (!c2).velocity.y} ;
          set_position (!c2) {x=stageright;y=(fst ((!c2).hitbox)).y} )
    else
      ()
  else
    if topdiff2 = 0 && (leftdiff2 > 0 || rightdiff2 > 0) then
      (if (!c2).stun > 0 then (!c2).jumps <- 1 else () ;
     (!c2).air <- true)
     else
     ()

(**[collide r1 r2] returns true if r1 and r2 overlap, false if not.
 r1 and r2 are of type rect from character. *)
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

(**[checkfordeath ch] checks if a character has been hit out of bounds of the stage
and processes their death if they are out of bounds. If a player loses all of their
lives, the game ends. This returns unit. *)
let checkfordeath ch =
  if !continue then
  (let i = if (!c1)= ch then 1 else 2 in
  if collide ({x=(-10000);y=800},{x=100000;y=1000000}) ch.hitbox then
    let x = if (fst ch.hitbox).x > 1000 then 1000 else
              if (fst ch.hitbox).x < 0 then 0 else
              (fst ch.hitbox).x in
    (Gui.start_blast x 600 true false i ;  if ch.lives = 1 then( continue := false ; gravitycounter := 0 ; ch.lives <- 0)else reset ch )
  else
    if collide ({x=(-100000);y=(-100000)},{x=100000;y=(-150)}) ch.hitbox then
      let x = if (fst ch.hitbox).x > 1000 then 1000 else
              if (fst ch.hitbox).x < 0 then 0 else
              (fst ch.hitbox).x in
      (Gui.start_blast x 0 true true i ; if ch.lives = 1 then( continue := false ; gravitycounter := 0; ch.lives <- 0) else reset ch)
    else
      if collide ({x=(-10000);y=(-10000)},{x=(-400);y=10000}) ch.hitbox then
        (Gui.start_blast 0 (fst ch.hitbox).y false true i ; if ch.lives = 1 then ( continue := false ; gravitycounter := 0; ch.lives <- 0)else reset ch)
    else
      if collide ({x=1400;y=(-10000)},{x=10000;y=100000}) ch.hitbox then
        (Gui.start_blast 1000 (fst ch.hitbox).y false false i ; if ch.lives = 1 then ( continue := false ; gravitycounter := 0; ch.lives <- 0)else reset ch)
      else
        ())
  else ()

(**[update ()] processes the change in position and velocity that occurs for each
character at every frame. This takes into account gravity, stage collisions,
momentum, and stuns. This returns unit.*)
let update () =
  let newpos1test = {x=(!c1).velocity.x + (fst ((!c1).hitbox)).x;
     y=(!c1).velocity.y + (fst ((!c1).hitbox)).y} in
  let newpos2test = {x=(!c2).velocity.x + (fst ((!c2).hitbox)).x;
     y=(!c2).velocity.y + (fst ((!c2).hitbox)).y} in
  stagecollision newpos1test newpos2test ; gravitycounter := !gravitycounter + 1 ;
  gravitycounter2 := !gravitycounter2 + 1 ; momentumcounter := !momentumcounter + 1 ;
  momentumcounter2 := !momentumcounter2 + 1 ; aicounter := !aicounter + 1 ;
  let newpos1 = {x=(!c1).velocity.x + (fst ((!c1).hitbox)).x;
     y=(!c1).velocity.y + (fst ((!c1).hitbox)).y} in
  let newpos2 = {x=(!c2).velocity.x + (fst ((!c2).hitbox)).x;
     y=(!c2).velocity.y + (fst ((!c2).hitbox)).y} in
  set_position (!c1) newpos1 ; set_position (!c2) newpos2 ;
  let newvy1 = if (!c1).air then
                 if (!c1).stun < 11 then
                   if (!c1).velocity.y = (!c1).speed * fallconstant then
                     (!c1).speed * fallconstant
                   else
                     if !gravitycounter mod 4 = 0 then
                       max ((!c1).velocity.y - gravity) ((!c1).speed * fallconstant/2)
                     else
                       (!c1).velocity.y
                 else (!c1).velocity.y
               else 0 in
  let newvy2 = if (!c2).air then
                 if (!c2).stun < 11 then
                   if (!c2).velocity.y = (!c2).speed * fallconstant then
                     (!c2).speed * fallconstant
                   else
                     if !gravitycounter2 mod 4 = 0 then
                       max ((!c2).velocity.y - gravity) ((!c2).speed * fallconstant/2)
                     else
                       (!c2).velocity.y
                 else (!c2).velocity.y
               else 0 in
  let newvx1 = if (!c1).stun > 0 then
    ((if (!c1).stun - 1 = 0 then stop_attack (!c1) else ()) ;
    (!c1).stun <- (!c1).stun -1 ;
    (!c1).velocity.x )
    else
      if !momentumcounter mod 10 = 0 then
        0
      else
      (!c1).velocity.x in
  let newvx2 = if (!c2).stun > 0 then
    ((if (!c2).stun - 1 = 0 then stop_attack (!c2) else ()) ;
    (!c2).stun <- (!c2).stun -1 ;
    (!c2).velocity.x )
    else
      if !momentumcounter2 mod 10 = 0 then
        0
      else
      (!c2).velocity.x in
  change_velocity (!c1) {x=newvx1;y=newvy1} ;
  change_velocity (!c2) {x=newvx2;y=newvy2} ;
  checkfordeath (!c1) ;
  checkfordeath (!c2) ;
  let (a,b,c,d) = !lastmove in
  lastmove := (a,b-1,c,d-1)

(**[process_attack attk i] checks if an attack attk from player i will hit the
opposing player. If it does, the opposing player is stunned and knocked back, among other effects.
attk is of type attack from the character module. This returns type unit.*)
let process_attack (a: attack) (i: int) : unit =
  let (ch,ch2) = if i = 0 then (!c1,!c2)
                 else (!c2,!c1) in
  let weight_modifier = 150 - (35*ch2.weight) in
  let _ = if ch.stun > 0 then () else
  start_attack ch a;
  match a with
  | Left ->
    (* Get a box that has width range and that is adjacent to the left of the character *)
    let p1 = (fst ch.hitbox) in (*Bottom left point of hitbox *)
    let p2 = (snd ch.hitbox) in  (*Top right point of hitbox *)
    let newp1 = {x=p1.x;y=p1.y} in
    let newp2 = {x=(p1.x+p2.x)/2;y=p2.y} in
    let attack_box = (newp1,newp2) in
    if collide attack_box ch2.hitbox then (*If the attack hits*)
      (get_hit ch2 10;
      (* This x value should be a function of dmg and attack strength *)
      change_velocity ch2
        {x=(-1)*(ch2.percent/4)*weight_modifier/100;
         y=((ch2.percent/4)*35)/100*weight_modifier/100};
      ch2.air <- true;
      stun ch2 knockbackstun;
      ())
    else
      ();
    (* Attacking character is also stunned *)
    stun ch 10
  | Right ->
    (* Get a box that has width range and that is adjacent to the left of the character *)
    let p1 = (fst ch.hitbox) in (*Bottom left point of hitbox *)
    let p2 = (snd ch.hitbox) in  (*Top right point of hitbox *)
    let newp1 = {x=(p1.x+p2.x)/2;y=p1.y} in
    let newp2 = {x=p2.x;y=p2.y} in
    let attack_box = (newp1,newp2) in
    if collide attack_box ch2.hitbox then (*If the attack hits*)
      (get_hit ch2 10;
      (* This x value should be a function of dmg and attack strength *)
      change_velocity ch2
        {x=ch2.percent/4*weight_modifier/100;
         y=((ch2.percent/4)*35)/100*weight_modifier/100};
      ch2.air <- true;
      stun ch2 knockbackstun;
      ())
    else
      ();
    (* Attacking character is also stunned *)
    stun ch 10
  | Up ->
    (* Get a box that has width range and that is adjacent to the left of the character *)
    let p1 = (fst ch.hitbox) in (*Bottom left point of hitbox *)
    let p2 = (snd ch.hitbox) in  (*Top right point of hitbox *)
    let newp1 = {x=p1.x;
                 y=p1.y + (2*((p2.y - p1.y)/3))} in
    let newp2 = {x=p2.x;
                 y=p2.y} in
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
    if collide attack_box1 ch2.hitbox &&
       collide attack_box2 ch2.hitbox &&
       collide attack_box3 ch2.hitbox then
      (get_hit ch2 10;
      (* This x value should be a function of dmg and attack strength *)
      change_velocity ch2
        {x=0;
         y=ch2.percent/4*weight_modifier/100};
      ch2.air <- true;
      stun ch2 knockbackstun;
      ())
    (*If the attack hits the left and middle box *)
    else if collide attack_box1 ch2.hitbox &&
      collide attack_box2 ch2.hitbox then
      (get_hit ch2 10;
      (* This x value should be a function of dmg and attack strength *)
      change_velocity ch2
        {x=(-1)*(ch2.percent/4)*weight_modifier/100;
         y=((ch2.percent/4)*173)/100*weight_modifier/100};
      ch2.air <- true;
      stun ch2 knockbackstun;
      ())
    (*If the attack hits the middle and right box *)
    else if collide attack_box2 ch2.hitbox &&
            collide attack_box3 ch2.hitbox then
      (get_hit ch2 10;
      (* This x value should be a function of dmg and attack strength *)
      change_velocity ch2
        {x=ch2.percent/4*weight_modifier/100;
         y=((ch2.percent/4)*173)/100*weight_modifier/100};
      ch2.air <- true;
      stun ch2 knockbackstun;
      ())
    (*If the attack hits the left box only *)
    else if collide attack_box1 ch2.hitbox then
      (get_hit ch2 10;
      (* This x value should be a function of dmg and attack strength *)
      change_velocity ch2
        {x=(-1)*(((ch2.percent/4)*173)/100)*weight_modifier/100;
         y=ch2.percent/4*weight_modifier/100};
      ch2.air <- true;
      stun ch2 knockbackstun;
      ())
    (*If the attack hits the right box only *)
    else if collide attack_box3 ch2.hitbox then
      (get_hit ch2 10;
      (* This x value should be a function of dmg and attack strength *)
      change_velocity ch2
        {x=((ch2.percent/4)*173)/100*weight_modifier/100;
         y=ch2.percent/4*weight_modifier/100};
      ch2.air <- true;
      stun ch2 knockbackstun;
      ())
    (*If the attack hits the middle box only *)
    else if collide attack_box2 ch2.hitbox then
      (get_hit ch2 10;
      (* This x value should be a function of dmg and attack strength *)
      change_velocity ch2
        {x=0;
         y=ch2.percent/4*weight_modifier/100};
      ch2.air <- true;
      stun ch2 knockbackstun;
      ())
    else
      ();
    (* Attacking character is also stunned *)

    stun ch 10
  (* Down needs to be fixed, right now it only works if character is in the air.
     A down attack on the ground should function differently, the area hit should be
     a horizontal rectangle on the bottom half of the character's hitbox.
     BOBBY - I think you should implement this same alternate way of calculating down attacks
     on the gound, to also up attacks on the ground. It's the same problem, right now
     an up attack with two people on the ground would never land because the collision
     rectangle would be above both their heads. *)
  | Down ->
    (* Get a box that has width range and that is adjacent to the left of the character *)
    let p1 = (fst ch.hitbox) in (*Bottom left point of hitbox *)
    let p2 = (snd ch.hitbox) in  (*Top right point of hitbox *)
    let newp1 = {x=p1.x;
                 y=p1.y} in
    let newp2 = {x=p2.x;
                 y=p1.y + ((p2.y - p1.y)/3)} in
    let attack_box = (newp1,newp2) in
    if collide attack_box ch2.hitbox then (*If the attack hits*)
      (get_hit ch2 10;
      if ch.air = true || ch2.air = true then
        (* This x value should be a function of dmg and attack strength *)
        change_velocity ch2 {x=0;
                             y=(-1)*(ch2.percent/4)*weight_modifier/100}
      else
        let ch2p1 = (fst ch2.hitbox) in
        let ch2p2 = (snd ch2.hitbox) in
        let x1 = (p1.x + p2.x) / 2 in
        let x2 = (ch2p1.x + ch2p2.x) / 2 in
        if x1 >= x2 then (* If ch1 is on the right side of ch2 *)
          change_velocity ch2 {x=(-1)*(ch2.percent/4)*weight_modifier/100;
                               y=0}
        else
          change_velocity ch2 {x=(ch2.percent/4)*weight_modifier/100;
                               y=0};
      stun ch2 knockbackstun;
      ())
    else
      ();
    (* Attacking character is also stunned *)
    stun ch 10
  in ()

(**[process_move move i] checks if a move move is valid and processes the changes to
the velocity of player i that result. move is of type move. *)
let process_move (m: move) (i: int) : unit =
  let ch = if i = 0 then  !c1 else !c2 in
  let _ = if ch.stun > 0 then () else
  match m with
  | MLeft ->
    if i = 0 then
      (let newv = {x=(!c1).speed * (-1);y=(!c1).velocity.y} in
      momentumcounter := 0 ;
      change_velocity (!c1) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (MLeft,12,c,d))
    else
      (let newv = {x=(!c2).speed * (-1);y=(!c2).velocity.y} in
      momentumcounter2 := 0 ;
      change_velocity (!c2) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (a,b,MLeft,12))
  | MRight ->
    if i = 0 then
      (let newv = {x=(!c1).speed;y=(!c1).velocity.y} in
      momentumcounter := 0 ;
      change_velocity (!c1) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (MRight,12,c,d))
    else
      (let newv = {x=(!c2).speed;y=(!c2).velocity.y} in
      momentumcounter2 := 0 ;
      change_velocity (!c2) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (a,b,MRight,12))
  | MDown ->
    if i = 0 then
      (let newv = {x=(!c1).velocity.x;y=(!c1).speed * fallconstant} in
      momentumcounter := 0 ;
      change_velocity (!c1) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (MDown,12,c,d))
    else
      (let newv = {x=(!c2).velocity.x;y=(!c2).speed * fallconstant} in
      momentumcounter2 := 0 ;
      change_velocity (!c2) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (a,b,MDown,12))
  | MUp ->
    if i = 0 then
      let (a,b,c,d) = !lastmove in
      let jumpconstant = if a = MUp && b > 0 then jumpconstant*2 else jumpconstant in
      let newv = {x=(!c1).velocity.x;y=(!c1).speed * jumpconstant} in
      if (!c1).jumps > 0 then
        (gravitycounter := 0 ;
        change_velocity (!c1) newv ;
        (!c1).jumps <- (!c1).jumps -1 ;
        (!c1).air <- true ;
        lastmove := (MUp,12,c,d))
      else
        ()
    else
      let (a,b,c,d) = !lastmove in
      let jumpconstant = if c = MUp && d>0 then jumpconstant*2 else jumpconstant in
      let newv = {x=(!c2).velocity.x;y=(!c2).speed * jumpconstant} in
      if (!c2).jumps > 0 then
        (gravitycounter2 := 0 ;
        change_velocity (!c2) newv ;
        (!c2).jumps <- (!c2).jumps -1 ;
        (!c2).air <- true ;
        lastmove := (a,b,MUp,12))
      else
        () in ()

(**[airesponse ()] generates responses from the ai in reaction to the current game
state and processes the attacks and moves for player 2. This is done by calling a
function in the Ai module. This returns unit. *)
let airesponse () =
  if !aicounter mod 5 = 0 then
    let r = Ai.execute_response_to_state (!c1) (!c2) in
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

(**[tickprocessor ()] maintains the frame rate of the game at 60 frames per second,
processes player inputs, generates ai responses, draws rhe current state by calling Gui methods,
 and processes character updates once per frame. This returns unit.*)
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
      ignore(Thread.create (fun x -> airesponse (); List.iter process x ; update ()) !newinputs) in
   newinputs := [] ;
   ignore(Thread.create (Gui.draw) (!c1,!c2));
   Thread.delay 0.02 ;
   if !continue  then tickprocessor () else
     if !gravitycounter = 30 then
       let i = if !c1.lives = 0 then 1 else 2 in
       Gui.draw_end i
     else tickprocessor ()

(**[input_loop ()] continuously reads inputs from the keyboard and adds them to a
list of inputs to be processed at the next frame. This returns unit.*)
let rec input_loop () =
  if !continue then
    let newchar = read_key () in
    newinputs := !newinputs @ [newchar] ;
    input_loop ()
  else ()

(**[start_engine ()] starts the game by calling intitial Gui draw methods, then
starts the loops to process inputs and game updates. Once game is finished, offers
player the option to replay or quit the game. This returns unit.*)
let rec start_engine () =
  continue := true ;
  let t = Thread.create input_loop () in
  Gui.setup_window();
  Gui.start_countdown 3 ;(**call 321 method from gui here.*)
  Thread.join (Thread.create tickprocessor ()) ;
  Thread.join t ;
  replay ()

and replay () =
  let newchar = read_key () in
  match newchar with
  | 'y' ->
    let (r1,r2) = chargen () in
    c1 := (create r1 {x=stageleft+100;y=stagetop}) ;
    c2 := (create r2 {x=stageright-100;y=stagetop}) ;
    start_engine ()
  | 'n' -> ()
  | _ -> replay ()

let _ = start_engine ()