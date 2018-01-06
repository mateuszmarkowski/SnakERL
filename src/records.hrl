-record(game, {
	pid,
	size_x,
	size_y,
	snakes = []
}).

-record(snake, {
	pid,
	direction,
	segments = []
}).

-record(segment, {
	position_x,
	position_y
}).
