open Engine

let stage_left = 150 
let stage_right = 1150

type reaction = Move | Attack | Recover

type position = LeftOf | RightOf | Above | Below | Inside

(*Not complete*)
let c1Position c1 ai = 
	if (fst c1.hitbox).y > (fst ai.hitbox).y then Above else
	if (fst c1.hitbox).y < (fst ai.hitbox).y then Below else
	if (fst c1.hitbox).x < (fst ai.hitbox).x then LeftOf else
	if (fst c1.hitbox).x > (fst ai.hitbox).x then RightOf else Inside

let move c1 ai = match (c1Position c1 ai) with
	| LeftOf -> process_move MLeft 1
	| RightOf -> process_move MRight 1
	| Above -> process_move MUp 1
	| Below -> process_move MDown 1
	| Inside -> ()

let attack c1 ai = match (c1Position c1 ai) with
	| LeftOf -> process_attack Left 1
	| RightOf -> process_attack Right 1
	| Above -> process_attack Up 1
	| Below -> process_attack Down 1
	| Inside -> process_attack Down 1

let recover c1 ai = 
	let ai_pos = (fst ai.hitbox) in 

	if ai.jumps <> 0 then process_move MUp 1 else 

	if (ai_pos.x <= stage_left) then process_move MRight 1 else 
		process_move MLeft 1

let react c1 ai = 
	let position_value_x = (fst c1.hitbox).x - (fst ai.hitbox).x in 
	let position_value_y = (fst c1.hitbox).y - (fst ai.hitbox).y
	let ai_pos = (fst ai.hitbox) in 

	if (ai_pos.x <= stage_left) || (ai_pos.x >= stage_right) then Recover else

	if ((position_value_x >= -(ai.range)) && (position_value_x <= ai.range))
		|| ((position_value_y <= ai.range) && (position_value_y >= -(ai.range))) then 
		Attack else
			Move

let execute_response_to_state c1 ai = match react c1 ai with
	| Move -> move c1 ai
	| Attack -> attack c1 ai
	| Recover -> recover c1 ai






