open Character

let stage_left = 150
let stage_right = 850


type reaction = Move | Attack | Recover | Spike

type position = LeftOf | RightOf | Above | Below | Inside

(*[csPosition c1 ai] is the position of c1 with regards to ai. 
	So if [csPosition c1 ai] returns LeftOf that means c1 is to the left of ai*)
let c1Position c1 ai =
	let pos_ya = (fst c1.hitbox).y - (snd ai.hitbox).y in
	let pos_yd = (snd c1.hitbox).y - (fst ai.hitbox).y in
	let pos_xl = (snd c1.hitbox).x - (fst ai.hitbox).x in
	let pos_xr = (fst c1.hitbox).x - (snd ai.hitbox).x in

	if (pos_ya >= 0) && ((((pos_xl >= -ai.range*3) && (pos_xr < 0)) || ((pos_xr <= ai.range*3) && (pos_xl > 0)))) then Above else
	if (pos_yd <= 0) && ((((pos_xl >= -ai.range*3) && (pos_xr < 0)) || ((pos_xr <= ai.range*3) && (pos_xl > 0)))) then Below else
	if (pos_xl < 0) && (pos_xr < 0) then LeftOf else
	if (pos_xr > 0) && (pos_xl > 0)then RightOf else Inside

(*[chooseAttack c1 ai] is the attack that ai should use.*)
let chooseAttack c1 ai = 
	if (fst c1.hitbox).y > (snd ai.hitbox).y then
		Above else
	if (snd c1.hitbox).y < (fst ai.hitbox).y then
		Below else
	if (fst c1.hitbox).x < (fst ai.hitbox).x then LeftOf else
	if (fst c1.hitbox).x > (fst ai.hitbox).x then RightOf else Inside

(*[move c1 ai] is the string representing which way the ai should move.
	"" if the ai shouldn't move.*)
let move c1 ai = match (c1Position c1 ai) with
	| LeftOf -> "ML"
	| RightOf -> "MR"
	| Above -> "MU"
	| Below -> "MD"
	| Inside -> ""

(*[attack c1 ai] is the string representing which way the ai should attack*)
let attack c1 ai = match (chooseAttack c1 ai) with
	| LeftOf -> "AL"
	| RightOf -> "AR"
	| Above -> "AU"
	| Below -> "AD"
	| Inside -> "AD"

(*[recover ai] is the string representing the way ai should move or jump 
	to get back to the stage*)
let recover ai =
	let ai_pos = (fst ai.hitbox) in

	if ai.jumps <> 0 then "MU" else

	if (ai_pos.x <= stage_left) then "MR" else
		"ML"

(*[spike c1 ai] is the string representing how the ai should go out off the stage
to hit c1*)
let spike c1 ai = 
	let pos_yd = (snd c1.hitbox).y - (fst ai.hitbox).y in
	let pos_xl = (snd c1.hitbox).x - (fst ai.hitbox).x in
	let pos_xr = (fst c1.hitbox).x - (snd ai.hitbox).x in
	if ((fst ai.hitbox).y < 200) && ((fst ai.hitbox).x < stage_left) then "MR" else
	if ((fst ai.hitbox).y < 200) && ((snd ai.hitbox).x > stage_right) then "ML" else
	if (pos_yd <= 200) && ((((pos_xl >= -ai.range/2) && (pos_xr < 0)) || ((pos_xr <= ai.range/2) && (pos_xl > 0)))) then
	"AD" else 
 	if ai.jumps = 2 then "MU" else
	if (pos_xl < 0) && (pos_xr < 0) then "ML" else
	if (pos_xr > 0) && (pos_xl > 0) then "MR" else
	""


(*[react c1 ai] returns type reaction which says how ai should react in the 
current situation*)
let react c1 ai = 
	(*negative when c1 is left of ai*)
	let pos_xl = (snd c1.hitbox).x - (fst ai.hitbox).x in 
	(*positive when c1 is right of ai*)
	let pos_xr = (fst c1.hitbox).x - (snd ai.hitbox).x in
	(*When characters are inside each other, xl and xr are opposite*)

	let ai_pos = (ai.hitbox) in
	let c1_pos = (c1.hitbox) in

	if ((fst c1_pos).x < stage_left) || ((snd c1_pos).x > stage_right) then Spike else

	if ((fst ai_pos).x <= stage_left) || ((snd ai_pos).x >= stage_right) then Recover else

	if (((pos_xl >= -ai.range) && (pos_xr < 0)) || ((pos_xr <= ai.range) && (pos_xl > 0)))
	 then Attack else
		Move

let execute_response_to_state c1 ai =

	match react c1 ai with
	| Move -> move c1 ai
	| Attack -> attack c1 ai
	| Recover -> recover ai
	| Spike -> spike c1 ai












