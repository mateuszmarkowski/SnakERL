-record(game, {
	pid,
	size_x,
	size_y,
	snakes = []
}).

-record(snake, {
	pid,
	direction,
	growth=0,
	segments = []
}).

-record(segment, {
	x,
	y
}).
