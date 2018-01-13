-record(game, {
	pid,
	state=pending,
	size_x,
	size_y,
	snakes = [],
	treasures = []
}).

-record(snake, {
	pid,
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
