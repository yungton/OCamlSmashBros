open Graphics
open Character

type attack = Left | Right | Down | Up | Neutral

type move = MLeft | MRight | MDown | MUp

(**let character = [create Light (100,100)]*)

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

let _ = start_engine ()