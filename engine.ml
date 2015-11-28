open Graphics
open Character

type attack = Left | Right | Down | Up | Neutral

type move = MLeft | MRight | MDown | MUp

let characters = (create Light {x=100;y=100},create Medium {x=100;y=100})

let lastmove = ref (MDown,1,MDown,1)

let fallconstant = -2

let gravity = 5

let jumpconstant = 1

let newinputs = ref []

let _ = open_graph ""

let update () = (**need to know when we hit stage, probably hardcoded coordinates of it vertically and horizontally*)
  let newpos1 = {x=(fst characters).velocity.x + (fst characters).pos.x;
     y=(fst characters).velocity.y + (fst characters).pos.y} in
  let newpos2 = {x=(snd characters).velocity.x + (snd characters).pos.x;
     y=(snd characters).velocity.y + (snd characters).pos.y} in
  moveto (fst characters) newpos1 ; moveto (snd characters) newpos2 ;
  let newvy1 = if (fst characters).air then
                 if (fst characters).velocity.y = (fst characters).speed * fallconstant then
                   (fst characters).speed * fallconstant
                 else
                   max ((fst characters).velocity.y - gravity) (fst characters).speed * fallconstant/2
               else 0 in
  let newvy2 = if (snd characters).air then
                 if (snd characters).velocity.y = (snd characters).speed * fallconstant then
                   (snd characters).speed * fallconstant
                 else
                  max ((snd characters).velocity.y - gravity) (snd characters).speed * fallconstant/2
               else 0 in
  change_velocity (fst characters) {x=0;y=newvy1} ;
  change_velocity (snd characters) {x=0;y=newvy2} ;
  let (a,b,c,d) = !lastmove in
  lastmove := (a,b-1,c,d-1)

let collide (r1: rect) (r2: rect) : bool =
  let r1p1 = fst r1 in
  let r1p2 = snd r1 in
  let r2p1 = fst r2 in
  let r2p2 = snd r2 in
  if

let process_attack (a: attack) (i: int) : unit =
  match a with
  | Left ->
    if i = 0 then
    (* Get a box that has width range and that is adjacent to the left of the character *)
      let p1 = (fst (fst characters).hitbox) in (*Top left point of hitbox *)
      let p2 = (snd (fst characters).hitbox) in  (*Bottom right point of hitbox *)
      let newp1 = {x=p1.x - (fst characters).range;y=p1.y} in
      let newp2 = {x=p1.x;y=p2.y} in
      let attack_box = (newp1,newp2) in
      if collide attack_box (snd characters).hitbox then (*If the attack hits*)
        ()
      else
        ()
    else
      ()
  | _ -> ()
  (*| Right ->
  | Up ->
  | Down ->
  | Neutral -> *)

let process_move (m: move) (i: int) : unit = (**consider stuns*)
  let _ =
  match m with
  | MLeft ->
    if i = 0 then
      (let newv = {x=(fst characters).speed * (-1);y=(fst characters).velocity.y} in
      change_velocity (fst characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (MLeft,12,c,d))
    else
      (let newv = {x=(snd characters).speed * (-1);y=(snd characters).velocity.y} in
      change_velocity (snd characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (a,b,MLeft,12))
  | MRight ->
    if i = 0 then
      (let newv = {x=(fst characters).speed;y=(fst characters).velocity.y} in
      change_velocity (fst characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (MRight,12,c,d))
    else
      (let newv = {x=(snd characters).speed;y=(snd characters).velocity.y} in
      change_velocity (snd characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (a,b,MRight,12))
  | MDown -> (**what about when on stage*)
    if i = 0 then
      (let newv = {x=(fst characters).velocity.x;y=(fst characters).speed * fallconstant} in
      change_velocity (fst characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (MDown,12,c,d))
    else
      (let newv = {x=(snd characters).velocity.x;y=(snd characters).speed * fallconstant} in
      change_velocity (snd characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (a,b,MDown,12))
  | MUp -> (**Should be setting jumps var in character*)
    if i = 0 then
      (let newv = {x=(fst characters).velocity.x;y=(fst characters).speed * jumpconstant} in
      change_velocity (fst characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (MUp,12,c,d))
    else
      (let newv = {x=(snd characters).velocity.x;y=(snd characters).speed * jumpconstant} in
      change_velocity (snd characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (a,b,MUp,12))


let rec tickprocessor () =
   let inputs = List.fold_right (fun x acc -> acc ^ (Char.escaped x)) !newinputs "" in
   print_endline inputs ;
   let process x =
     match x with
     | 'a' -> process_move MLeft 0
     | 'w' -> process_move MUp 0
     | 's' -> process_move MDown 0
     | 'd' -> process_move MRight 0
     | _ -> () in
   let _ =
      ignore(Thread.create (fun x -> let _ = List.iter process x in update ()) !newinputs) in
   newinputs := [] ;
   Thread.delay 0.017 ;
   tickprocessor ()

let rec input_loop () =
  let newchar = read_key () in
  newinputs := !newinputs @ [newchar] ;
  input_loop ()

let start_engine () =
  let _ = Thread.create input_loop () in
  Thread.join (Thread.create tickprocessor ())

let _ = start_engine ()