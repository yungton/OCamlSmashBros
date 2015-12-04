open Character

let stage_left = 150
let stage_right = 850


type reaction = Move | Attack | Recover | Spike

type position = LeftOf | RightOf | Above | Below | Inside


let c1Position c1 ai =
	let pos_ya = (fst c1.hitbox).y - (snd ai.hitbox).y in
	let pos_yd = (snd c1.hitbox).y - (fst ai.hitbox).y in
	let pos_xl = (snd c1.hitbox).x - (fst ai.hitbox).x in
	let pos_xr = (fst c1.hitbox).x - (snd ai.hitbox).x in

	if (pos_ya >= 0) && ((((pos_xl >= -ai.range*3) && (pos_xr < 0)) || ((pos_xr <= ai.range*3) && (pos_xl > 0)))) then Above else
	if (pos_yd <= 0) && ((((pos_xl >= -ai.range*3) && (pos_xr < 0)) || ((pos_xr <= ai.range*3) && (pos_xl > 0)))) then Below else
	if (pos_xl < 0) && (pos_xr < 0) then LeftOf else
	if (pos_xr > 0) && (pos_xl > 0)then RightOf else Inside

let chooseAttack c1 ai = 
	
	(*let position_value_x = (fst c1.hitbox).x - (fst ai.hitbox).x in *)

	if (fst c1.hitbox).y > (snd ai.hitbox).y (*&&
		((position_value_x <= ai.range/2) && (position_value_x >= -(ai.range/2)))*) then
		Above else
	if (snd c1.hitbox).y < (fst ai.hitbox).y (*&&
		(position_value_x <= ai.range/2 && position_value_x >= -(ai.range/2))*) then
		Below else
	if (fst c1.hitbox).x < (fst ai.hitbox).x then LeftOf else
	if (fst c1.hitbox).x > (fst ai.hitbox).x then RightOf else Inside

let move c1 ai = match (c1Position c1 ai) with
	| LeftOf -> "ML"
	| RightOf -> "MR"
	| Above -> "MU"
	| Below -> "MD"
	| Inside -> ""

let attack c1 ai = match (chooseAttack c1 ai) with
	| LeftOf -> "AL"
	| RightOf -> "AR"
	| Above -> "AU"
	| Below -> "AD"
	| Inside -> "AD"

let recover c1 ai =
	let ai_pos = (fst ai.hitbox) in

	if ai.jumps <> 0 then "MU" else

	if (ai_pos.x <= stage_left) then "MR" else
		"ML"

let spike c1 ai = 
	let pos_yd = (snd c1.hitbox).y - (fst ai.hitbox).y in
	let pos_xl = (snd c1.hitbox).x - (fst ai.hitbox).x in
	let pos_xr = (fst c1.hitbox).x - (snd ai.hitbox).x in
	if (pos_yd <= 200) && ((((pos_xl >= -ai.range/2) && (pos_xr < 0)) || ((pos_xr <= ai.range/2) && (pos_xl > 0)))) then
	"AD" else 
 if ai.jumps <> 0 then "MU" else
	if (pos_xl < 0) && (pos_xr < 0) then "ML" else
	if (pos_xr > 0) && (pos_xl > 0) then "MR" else
	""



let react c1 ai = 
	(*negative when c1 is left of ai*)
	let pos_xl = (snd c1.hitbox).x - (fst ai.hitbox).x in 
	(*positive when c1 is right of ai*)
	let pos_xr = (fst c1.hitbox).x - (snd ai.hitbox).x in
	(*let position_value_ya = (fst c1.hitbox).y - (snd ai.hitbox).y in 
	let position_value_yd = (snd c1.hitbox).y - (fst ai.hitbox).y in*) 
	(*When characters are inside each other, xl and xr are opposite*)

	(*let position_value_x = (fst c1.hitbox).x - (fst ai.hitbox).x in 
>>>>>>> Stashed changes
	let position_value_y = (fst c1.hitbox).y - (fst ai.hitbox).y in
	let ai_pos = (fst ai.hitbox) in

	if (ai_pos.x <= stage_left) || (ai_pos.x >= stage_right) then Recover else

<<<<<<< Updated upstream
	if ((position_value_x >= -(ai.range)) && (position_value_x <= ai.range))
		|| ((position_value_y <= ai.range) && (position_value_y >= -(ai.range)))
			&& (position_value_x <= ai.range/2 && position_value_x >= -(ai.range/2)) then
=======
	if ((position_value_xl >= -(ai.range)) && (position_value_xr <= ai.range))
		|| ((position_value_y <= ai.range) && (position_value_y >= -(ai.range))) 
			&& (position_value_x <= ai.range/2 && position_value_x >= -(ai.range/2)) then 
>>>>>>> Stashed changes
		Attack else
			Move*)
	(*if (((position_value_xl >= -ai.range) && (position_value_xr < 0)) || ((position_value_xr >= -ai.range) && (position_value_xl > 0)))
		|| (((position_value_ya <= ai.range/2) || (position_value_yd >= -ai.range/2)) 
			&& (((position_value_xl <= -ai.range) && (position_value_xr < 0)) || ((position_value_xr >= -ai.range) && (position_value_xl > 0))))
		then 
		Attack else
		Move*)
	let ai_pos = (ai.hitbox) in
	let c1_pos = (c1.hitbox) in
	(*If c1 is offstage, then spike*)
	if ((fst c1_pos).x < stage_left) || ((snd c1_pos).x > stage_right) then Spike else
	if ((fst ai_pos).x <= stage_left) || ((snd ai_pos).x >= stage_right) then Recover else

	if (((pos_xl >= -ai.range) && (pos_xr < 0)) || ((pos_xr <= ai.range) && (pos_xl > 0)))
	
	 then
		Attack else
		Move

let execute_response_to_state c1 ai =

	match react c1 ai with
	| Move -> move c1 ai
	| Attack -> attack c1 ai
	| Recover -> recover c1 ai
	| Spike -> spike c1 ai












