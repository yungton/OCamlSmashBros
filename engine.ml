open Graphics
open Character
(**ai attack off stage, down smash on ground out, gui methods for replay,countdown,
  attack needs weight consideration, mess with ranges, clean code
   *)

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

let continue = ref true

let newinputs = ref []

let _ = Random.self_init ()

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

let characters = ref (create rand1 {x=stageleft+100;y=stagetop},
                  create rand2 {x=stageright-100;y=stagetop})

let ch1width = get_width (fst !characters)
let ch2width = get_width (snd !characters)

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
      ((fst !characters).air <- false ;
      (fst !characters).jumps <- 2 ;
      change_velocity (fst !characters) {x=0;y=0} ;
      set_position (fst !characters) {x=pos1.x;y=stagetop})
      else
        if leftdiff1 > rightdiff1 then
          (change_velocity (fst !characters) {x=0;y= (fst !characters).velocity.y} ;
          set_position (fst !characters) {x=stageleft-ch1width;y=(fst ((fst !characters).hitbox)).y} )
        else
          (change_velocity (fst !characters) {x=0;y= (fst !characters).velocity.y} ;
          set_position (fst !characters) {x=stageright;y=(fst ((fst !characters).hitbox)).y} )
    else
      ()
  else
    if topdiff1 = 0 && (leftdiff1 > 0 || rightdiff1 > 0) then
      (if (fst !characters).stun > 0 then (fst !characters).jumps <- 1 else () ;
     (fst !characters).air <- true)
     else
     ()) ;
  if topdiff2 < 0 then
    if leftdiff2 < 0 && rightdiff2 < 0 then
      if topdiff2 > leftdiff2 && topdiff2 > rightdiff2 then
      ((snd !characters).air <- false ;
      (snd !characters).jumps <- 2 ;
      change_velocity (snd !characters) {x=0;y=0} ;
      set_position (snd !characters) {x=pos2.x;y=stagetop})
      else
        if leftdiff2 > rightdiff2 then
          (change_velocity (snd !characters) {x=0;y= (snd !characters).velocity.y} ;
          set_position (snd !characters) {x=stageleft-ch2width;y=(fst ((snd !characters).hitbox)).y} )
        else
          (change_velocity (snd !characters) {x=0;y= (snd !characters).velocity.y} ;
          set_position (snd !characters) {x=stageright;y=(fst ((snd !characters).hitbox)).y} )
    else
      ()
  else
    if topdiff2 = 0 && (leftdiff2 > 0 || rightdiff2 > 0) then
      (if (snd !characters).stun > 0 then (snd !characters).jumps <- 1 else () ;
     (snd !characters).air <- true)
     else
     ()

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

let checkfordeath ch =
  if collide ({x=(-10000);y=1000},{x=100000;y=1000000}) ch.hitbox || collide ({x=(-100000);y=(-100000)},{x=100000;y=(-150)}) ch.hitbox
     || collide ({x=(-10000);y=(-10000)},{x=(-500);y=10000}) ch.hitbox || collide ({x=1500;y=(-10000)},{x=10000;y=100000}) ch.hitbox then
    (reset ch ; if ch.lives = 0 then continue := false else ())
  else ()

let update () =
  let newpos1test = {x=(fst !characters).velocity.x + (fst ((fst !characters).hitbox)).x;
     y=(fst !characters).velocity.y + (fst ((fst !characters).hitbox)).y} in
  let newpos2test = {x=(snd !characters).velocity.x + (fst ((snd !characters).hitbox)).x;
     y=(snd !characters).velocity.y + (fst ((snd !characters).hitbox)).y} in
  stagecollision newpos1test newpos2test ; gravitycounter := !gravitycounter + 1 ;
  gravitycounter2 := !gravitycounter2 + 1 ; momentumcounter := !momentumcounter + 1 ;
  momentumcounter2 := !momentumcounter2 + 1 ; aicounter := !aicounter + 1 ;
  let newpos1 = {x=(fst !characters).velocity.x + (fst ((fst !characters).hitbox)).x;
     y=(fst !characters).velocity.y + (fst ((fst !characters).hitbox)).y} in
  let newpos2 = {x=(snd !characters).velocity.x + (fst ((snd !characters).hitbox)).x;
     y=(snd !characters).velocity.y + (fst ((snd !characters).hitbox)).y} in
  set_position (fst !characters) newpos1 ; set_position (snd !characters) newpos2 ;
  let newvy1 = if (fst !characters).air then
                 if (fst !characters).stun < 11 then
                   if (fst !characters).velocity.y = (fst !characters).speed * fallconstant then
                     (fst !characters).speed * fallconstant
                   else
                     if !gravitycounter mod 4 = 0 then
                       max ((fst !characters).velocity.y - gravity) ((fst !characters).speed * fallconstant/2)
                     else
                       (fst !characters).velocity.y
                 else (fst !characters).velocity.y
               else 0 in
  let newvy2 = if (snd !characters).air then
                 if (snd !characters).stun < 11 then
                   if (snd !characters).velocity.y = (snd !characters).speed * fallconstant then
                     (snd !characters).speed * fallconstant
                   else
                     if !gravitycounter2 mod 4 = 0 then
                       max ((snd !characters).velocity.y - gravity) ((snd !characters).speed * fallconstant/2)
                     else
                       (snd !characters).velocity.y
                 else (snd !characters).velocity.y
               else 0 in
  let newvx1 = if (fst !characters).stun > 0 then
    ((if (fst !characters).stun - 1 = 0 then stop_attack (fst !characters) else ()) ;
    (fst !characters).stun <- (fst !characters).stun -1 ;
    (fst !characters).velocity.x )
    else
      if !momentumcounter mod 10 = 0 then
        0
      else
      (fst !characters).velocity.x in
  let newvx2 = if (snd !characters).stun > 0 then
    ((if (snd !characters).stun - 1 = 0 then stop_attack (snd !characters) else ()) ;
    (snd !characters).stun <- (snd !characters).stun -1 ;
    (snd !characters).velocity.x )
    else
      if !momentumcounter2 mod 10 = 0 then
        0
      else
      (snd !characters).velocity.x in
  change_velocity (fst !characters) {x=newvx1;y=newvy1} ;
  change_velocity (snd !characters) {x=newvx2;y=newvy2} ;
  checkfordeath (fst !characters) ;
  checkfordeath (snd !characters) ;
  let (a,b,c,d) = !lastmove in
  lastmove := (a,b-1,c,d-1)

let process_attack (a: attack) (i: int) : unit =
  let (ch,ch2) = if i = 0 then (fst !characters,snd !characters)
                 else (snd !characters,fst !characters) in
  let weight_modifier = 150 - (25*ch2.weight) in
  let _ = if ch.stun > 0 then () else
  start_attack ch a;
  match a with
  | Left ->
    (* Get a box that has width range and that is adjacent to the left of the character *)
    let p1 = (fst ch.hitbox) in (*Bottom left point of hitbox *)
    let p2 = (snd ch.hitbox) in  (*Top right point of hitbox *)
    let newp1 = {x=p1.x - ch.range;y=p1.y} in
    let newp2 = {x=p1.x;y=p2.y} in
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
    let newp1 = {x=p2.x;y=p1.y} in
    let newp2 = {x=p2.x + ch.range;y=p2.y} in
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
    let newp1 = {x=p1.x - (ch.range/2);
                 y=p1.y + ((p2.y - p1.y)/3)} in
    let newp2 = {x=p2.x + (ch.range/2);
                 y=p2.y + (ch.range/2)} in
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
    let newp1 = {x=p1.x - (ch.range/2);
                 y=p1.y - (ch.range/2)} in
    let newp2 = {x=p2.x + (ch.range/2);
                 y=p1.y + ((p2.y - p1.y)/3)} in
    let attack_box = (newp1,newp2) in
    if collide attack_box ch2.hitbox then (*If the attack hits*)
      (get_hit ch2 10;
      if ch.air = true then
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

let process_move (m: move) (i: int) : unit =
  let ch = if i = 0 then  fst !characters else snd !characters in
  let _ = if ch.stun > 0 then () else
  match m with
  | MLeft ->
    if i = 0 then
      (let newv = {x=(fst !characters).speed * (-1);y=(fst !characters).velocity.y} in
      momentumcounter := 0 ;
      change_velocity (fst !characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (MLeft,12,c,d))
    else
      (let newv = {x=(snd !characters).speed * (-1);y=(snd !characters).velocity.y} in
      momentumcounter2 := 0 ;
      change_velocity (snd !characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (a,b,MLeft,12))
  | MRight ->
    if i = 0 then
      (let newv = {x=(fst !characters).speed;y=(fst !characters).velocity.y} in
      momentumcounter := 0 ;
      change_velocity (fst !characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (MRight,12,c,d))
    else
      (let newv = {x=(snd !characters).speed;y=(snd !characters).velocity.y} in
      momentumcounter2 := 0 ;
      change_velocity (snd !characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (a,b,MRight,12))
  | MDown ->
    if i = 0 then
      (let newv = {x=(fst !characters).velocity.x;y=(fst !characters).speed * fallconstant} in
      momentumcounter := 0 ;
      change_velocity (fst !characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (MDown,12,c,d))
    else
      (let newv = {x=(snd !characters).velocity.x;y=(snd !characters).speed * fallconstant} in
      momentumcounter2 := 0 ;
      change_velocity (snd !characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (a,b,MDown,12))
  | MUp ->
    if i = 0 then
      let (a,b,c,d) = !lastmove in
      let jumpconstant = if a = MUp && b > 0 then jumpconstant*2 else jumpconstant in
      let newv = {x=(fst !characters).velocity.x;y=(fst !characters).speed * jumpconstant} in
      if (fst !characters).jumps > 0 then
        (gravitycounter := 0 ;
        change_velocity (fst !characters) newv ;
        (fst !characters).jumps <- (fst !characters).jumps -1 ;
        (fst !characters).air <- true ;
        lastmove := (MUp,12,c,d))
      else
        ()
    else
      let (a,b,c,d) = !lastmove in
      let jumpconstant = if c = MUp && d>0 then jumpconstant*2 else jumpconstant in
      let newv = {x=(snd !characters).velocity.x;y=(snd !characters).speed * jumpconstant} in
      if (snd !characters).jumps > 0 then
        (gravitycounter2 := 0 ;
        change_velocity (snd !characters) newv ;
        (snd !characters).jumps <- (snd !characters).jumps -1 ;
        (snd !characters).air <- true ;
        lastmove := (a,b,MUp,12))
      else
        () in ()

let airesponse () =
  if !aicounter mod 1 = 0 then
    let r = Ai.execute_response_to_state (fst !characters) (snd !characters) in
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
      ignore(Thread.create (fun x -> airesponse (); List.iter process x ; update ()) !newinputs) in
   newinputs := [] ;
   ignore(Thread.create (Gui.draw_characters) !characters);
   Thread.delay 0.02 ;
   if !continue then tickprocessor () else ()

let rec input_loop () =
  let newchar = read_key () in
  newinputs := !newinputs @ [newchar] ;
  if !continue then input_loop () else ()

let rec start_engine () =
  continue := true ;
  let t = Thread.create input_loop () in
  Gui.setup_window();
  Gui.draw !characters; (**call 321 method from gui here.*)
  Thread.join (Thread.create tickprocessor ()) ;
  Thread.join t ;
  replay ()

and replay () =
  (**call gui finish method here, 'y' for yes, 'n' for no*)
  let newchar = read_key () in
  match newchar with
  | 'y' ->
    let (r1,r2) = chargen () in
    characters := (create r1 {x=stageleft+100;y=stagetop},
                  create r2 {x=stageright-100;y=stagetop}) ;
    start_engine ()
  | 'n' -> ()
  | _ -> replay ()

let _ = start_engine ()



