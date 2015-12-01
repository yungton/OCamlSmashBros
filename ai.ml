open Character

let stage_left = 150 
let stage_right = 850

type reaction = Move | Attack | Recover

type position = LeftOf | RightOf | Above | Below | Inside


let c1Position c1 ai = 
	if (fst c1.hitbox).y > (fst ai.hitbox).y then Above else
	if (fst c1.hitbox).y < (fst ai.hitbox).y then Below else
	if (fst c1.hitbox).x < (fst ai.hitbox).x then LeftOf else
	if (fst c1.hitbox).x > (fst ai.hitbox).x then RightOf else Inside

let chooseAttack c1 ai = 
	let position_value_x = (fst c1.hitbox).x - (fst ai.hitbox).x in 

	if (fst c1.hitbox).y > (fst ai.hitbox).y && 
		(position_value_x <= ai.range/2 && position_value_x >= -(ai.range/2)) then
		Above else
	if (fst c1.hitbox).y < (fst ai.hitbox).y && 
		(position_value_x <= ai.range/2 && position_value_x >= -(ai.range/2)) then
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

let react c1 ai = 
	let position_value_x = (fst c1.hitbox).x - (fst ai.hitbox).x in 
	let position_value_y = (fst c1.hitbox).y - (fst ai.hitbox).y in
	let ai_pos = (fst ai.hitbox) in 

	if (ai_pos.x <= stage_left) || (ai_pos.x >= stage_right) then Recover else

	if ((position_value_x >= -(ai.range)) && (position_value_x <= ai.range))
		|| ((position_value_y <= ai.range) && (position_value_y >= -(ai.range))) 
			&& (position_value_x <= ai.range/2 && position_value_x >= -(ai.range/2)) then 
		Attack else
			Move

let execute_response_to_state c1 ai = 

	match react c1 ai with
	| Move -> move c1 ai 
	| Attack -> attack c1 ai
	| Recover -> recover c1 ai






