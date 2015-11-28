open Graphics
open Character

type attack = Left | Right | Down | Up | Neutral

type move = MLeft | MRight | MDown | MUp

(*let characters = (create Light (100,100),create Medium (100,100))*)

let lastmove = ref [None;None]

let newinputs = ref []

let _ = open_graph ""

let rec tickprocessor () =
   let inputs = List.fold_right (fun x acc -> acc ^ (Char.escaped x)) !newinputs "" in
   print_endline inputs ;
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

let process_move (m: move) (i: int) : unit =
  match m with
  | MLeft ->
    if i = 0 then
      ((fst characters).velocity <-
        ((fst characters).speed * (-1),snd (fst characters).velocity);
      ())
    else
      ((snd characters).velocity <-
        ((snd characters).speed * (-1),snd (snd characters).velocity);
      ())
  | MRight -> failwith "TODO"
  | _ -> failwith "TODO"

let update () =
  (fst characters).pos <-
    (fst (fst characters).velocity + fst (fst characters).pos,
     snd (fst characters).velocity + snd (fst characters).pos);
  (snd characters).pos <-
    (fst (snd characters).velocity + fst (snd characters).pos,
     snd (snd characters).velocity + snd (snd characters).pos);
  ()


let _ = start_engine ()