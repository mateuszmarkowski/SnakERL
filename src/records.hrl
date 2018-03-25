-record(game, {
	pid,
	name,
	state=pending,
	max_snakes,
	size_x,
	size_y,
	snakes = [],
	treasures = []
}).

-record(snake, {
	pid,
	name,
	state=active,
	direction,
	growth=0,
	segments = []
}).

-record(segment, {
	x,
	y
}).

-record(treasure, {
	type,
	x,
	y
}).
