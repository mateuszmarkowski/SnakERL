-record(game, {
	pid,
	state=pending,
	size_x,
	size_y,
	snakes = []
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
