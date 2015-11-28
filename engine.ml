open Graphics
open Character

type attack = Left | Right | Down | Up | Neutral

type move = MLeft | MRight | MDown | MUp

(*let characters = (create Light (100,100),create Medium (100,100))*)

let lastmove = ref (MDown,1,MDown,1)

let fallconstant = -2

let gravity = 5

let jumpconstant = 1

let newinputs = ref []

let _ = open_graph ""

let update () = (**need to know when we hit stage, probably hardcoded coordinates of it vertically and horizontally*)
  let newpos1 = (fst (fst characters).velocity + fst (fst characters).pos,
     snd (fst characters).velocity + snd (fst characters).pos) in
  let newpos2 = (fst (snd characters).velocity + fst (snd characters).pos,
     snd (snd characters).velocity + snd (snd characters).pos) in
  moveto (fst characters) newpos1 ; moveto (snd characters) newpos2 ;
  let newvy1 = if (fst characters).air then
                 if snd (fst characters).velocity = (fst characters).speed * fallconstant then
                   (fst characters).speed * fallconstant
                 else
                   max ((snd (fst characters).velocity) - gravity) (fst characters).speed * fallconstant/2
               else 0 in
  let newvy2 = if (snd characters).air then
                 if snd (snd characters).velocity = (snd characters).speed * fallconstant then
                   (snd characters).speed * fallconstant
                 else
                  max ((snd (snd characters).velocity) - gravity) (snd characters).speed * fallconstant/2
               else 0 in
  change_velocity (fst characters) (0,newvy1) ;
  change_velocity (snd characters) (0 newvy2) ;
  let (a,b,c,d) = !lastmove in
  lastmove := (a,b-1,c,d-1)

let process_move (m: move) (i: int) : unit = (**consider stuns*)
  let _ =
  match m with
  | MLeft ->
    if i = 0 then
      (let newv = ((fst characters).speed * (-1),snd (fst characters).velocity) in
      change_velocity (fst characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (MLeft,12,c,d))
    else
      (let newv = ((snd characters).speed * (-1),snd (snd characters).velocity) in
      change_velocity (snd characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (a,b,MLeft,12))
  | MRight ->
    if i = 0 then
      (let newv = ((fst characters).speed ,snd (fst characters).velocity) in
      change_velocity (fst characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (MRight,12,c,d))
    else
      (let newv = ((snd characters).speed,snd (snd characters).velocity) in
      change_velocity (snd characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (a,b,MRight,12))
  | MDown -> (**what about when on stage*)
    if i = 0 then
      (let newv = (fst (fst characters).velocity,(fst characters).speed * fallconstant) in
      change_velocity (fst characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (MDown,12,c,d))
    else
      (let newv = (fst (snd characters).velocity,(snd characters).speed * fallconstant) in
      change_velocity (snd characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (a,b,MDown,12))
  | MUp -> (**Should be setting jumps var in character*)
    if i = 0 then
      (let newv = (fst (fst characters).velocity,(fst characters).speed * jumpconstant) in
      change_velocity (fst characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (MUp,12,c,d))
    else
      (let newv = (fst (snd characters).velocity,(snd characters).speed * jumpconstant) in
      change_velocity (snd characters) newv ;
      let (a,b,c,d) = !lastmove in
      lastmove := (a,b,MUp,12)) in
    update ()


let rec tickprocessor () =
   let inputs = List.fold_right (fun x acc -> acc ^ (Char.escaped x)) !newinputs "" in
   print_endline inputs ;
   let process x =
     match x with
     | 'a' -> process_move MLeft 0
     | 'w' -> process_move MUp 0
     | 's' -> process_move MDown 0
     | 'd' -> process_move MRight 0
     | _ -> update () in
   let _ = if !newinputs = [] then
      ignore(Thread.create update ())
    else
      ignore(Thread.create (List.iter process) !newinputs) in
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