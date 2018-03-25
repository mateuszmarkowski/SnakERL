-module(snake_physics).

-include("records.hrl").

-define(DIRECTION_UP, 1).
-define(DIRECTION_RIGHT, 2).
-define(DIRECTION_DOWN, 3).
-define(DIRECTION_LEFT, 4).

-export([detect_consumptions/1, detect_collisions/1, strip_colliding_snakes/1, maybe_change_direction/3, add_treasure/2, move_snakes/1]).

-ifdef(EUNIT).
-export([move_segments/4, move_snake/3]).
-endif.

detect_consumptions(Game = #game{snakes = Snakes, treasures = Treasures}) ->
	detect_consumptions(Game, Snakes, [], Treasures).
	
detect_consumptions(Game, [], AnalyzedSnakes, Treasures) ->
	Game#game{snakes = AnalyzedSnakes, treasures = Treasures};

detect_consumptions(Game = #game{size_x = SizeX, size_y = SizeY}, [Snake = #snake{direction = Direction, segments = [#segment{x = X, y = Y}|_Segments]}|Snakes], AnalyzedSnakes, Treasures) ->
	{NewX, NewY} = calculate_new_x_y(X, Y, Direction, SizeX, SizeY),
	
	Treasure = lists:filter(fun(#treasure{x = X2, y = Y2}) -> (X2 =:= NewX) and (Y2 =:= NewY) end, Treasures),
	
	case length(Treasure) of 
		1 -> detect_consumptions(Game, Snakes, [Snake#snake{growth = Snake#snake.growth + 1}|AnalyzedSnakes], lists:filter(fun(#treasure{x = X2, y = Y2}) -> (X2 =/= NewX) or (Y2 =/= NewY) end, Treasures));
		_ -> detect_consumptions(Game, Snakes, [Snake|AnalyzedSnakes], Treasures)
	end.

add_treasure(Game = #game{treasures = Treasures}, Type) ->
	{X, Y} = get_free_random_spot(Game),
	Game#game{treasures = [#treasure{type = Type, x = X, y = Y}|Treasures]}.

strip_colliding_snakes(Snakes) ->	
	[Snake || Snake = #snake{state = State} <- Snakes, State =/= collision].

%% Snakes can't move backwards!
maybe_change_direction(#game{size_x = SizeX, size_y = SizeY}, Snake = #snake{segments = Segments}, Direction) when length(Segments) > 1 ->
	%% The first segment is the head of our snake.
	Segment1 = lists:nth(1, Segments),
	Segment2 = lists:nth(2, Segments),
	
	{NewX, NewY} = calculate_new_x_y(Segment1#segment.x, Segment1#segment.y, Direction, SizeX, SizeY),
	
	Collides = (Segment2#segment.x =:= NewX) and (Segment2#segment.y =:= NewY),
	
	case Collides of
		true -> Snake;
		_ -> Snake#snake{direction = Direction}
	end;
	
maybe_change_direction(_Game, Snake, Direction) ->
	Snake#snake{direction = Direction, growth = 1}.

move_snakes(Game) ->
	move_snakes(Game, Game#game.snakes, []). 

move_snakes(Game, [], MovedSnakes) ->
	Game#game{snakes = lists:reverse(MovedSnakes)};

move_snakes(Game = #game{size_x = SizeX, size_y = SizeY}, [Snake|Snakes], MovedSnakes) ->
	move_snakes(Game, Snakes, [move_snake(Snake, SizeX, SizeY)|MovedSnakes]).

move_snake(Snake = #snake{growth = Growth, direction = Direction, segments = [Segment = #segment{x = X, y = Y}|Segments]}, SizeX, SizeY) ->
	ShouldGrow = Growth > 0,
	
	%% Snakes grow one segment per tick as long as there're pending segments (Growth - number of pending segments)
	Growth2 = case Growth of
		G when G > 1 -> G - 1;
		_ -> 0
	end,
	
	{NewX, NewY} = calculate_new_x_y(X, Y, Direction, SizeX, SizeY),
	
	Segments2 = move_segments(NewX, NewY, [Segment] ++ Segments, ShouldGrow),
	
	Snake#snake{segments = Segments2, growth = Growth2}.

%% looping
calculate_new_x_y(X, Y, Direction, _SizeX, SizeY) when Direction =:= ?DIRECTION_UP, Y =:= 0 -> {X, SizeY - 1};

calculate_new_x_y(X, Y, Direction, _SizeX, _SizeY) when Direction =:= ?DIRECTION_UP    -> {X, Y - 1};

%% looping
calculate_new_x_y(X, Y, Direction, SizeX, _SizeY) when Direction =:= ?DIRECTION_RIGHT, X + 1 =:= SizeX -> {0, Y};

calculate_new_x_y(X, Y, Direction, _SizeX, _SizeY) when Direction =:= ?DIRECTION_RIGHT -> {X + 1, Y};

%% looping
calculate_new_x_y(X, Y, Direction, _SizeX, SizeY) when Direction =:= ?DIRECTION_DOWN, Y + 1 =:= SizeY  -> {X, 0};

calculate_new_x_y(X, Y, Direction, _SizeX, _SizeY) when Direction =:= ?DIRECTION_DOWN  -> {X, Y + 1};

%% looping
calculate_new_x_y(X, Y, Direction, SizeX, _SizeY) when Direction =:= ?DIRECTION_LEFT, X =:= 0  -> {SizeX - 1, Y};

calculate_new_x_y(X, Y, Direction, _SizeX, _SizeY) when Direction =:= ?DIRECTION_LEFT  -> {X - 1, Y}.

detect_collisions(#game{snakes = Snakes}) ->
	detect_collisions(Snakes, Snakes, []).
	
detect_collisions([], _AllSnakes, AnalyzedSnakes) ->
	AnalyzedSnakes;

detect_collisions([Snake = #snake{pid = Pid, segments = [Segment|_Segments]}|RemainingSnakes], AllSnakes, AnalyzedSnakes) ->
	case does_collide(Segment, get_all_segments(AllSnakes, Pid)) of	
		true -> detect_collisions(RemainingSnakes, AllSnakes, [Snake#snake{state = collision}|AnalyzedSnakes]);
		false -> detect_collisions(RemainingSnakes, AllSnakes, [Snake|AnalyzedSnakes])
	end.

does_collide(_HeadSegment, []) -> false;

does_collide(#segment{x = HX, y = HY}, [#segment{x = X, y = Y}|_Segments]) when HX == X, HY == Y -> true;

does_collide(HeadSegment, [_Segment|Segments]) ->
	does_collide(HeadSegment, Segments).

%% Get a list of all occupied pixels except for the pixel occupied by the head of the snake identified by SkipHeadPid.
get_all_segments(AllSnakes, SkipHeadPid) when is_pid(SkipHeadPid) ->
	get_all_segments(AllSnakes, [], SkipHeadPid).

get_all_segments([], Segments, _SkipHeadPid) ->
	Segments;

get_all_segments([#snake{pid = Pid, segments = [_HeadSegment|CurrentSegments]}|Snakes], Segments, SkipHeadPid) when Pid == SkipHeadPid ->
	get_all_segments(Snakes, CurrentSegments ++ Segments, SkipHeadPid);

get_all_segments([#snake{segments = CurrentSegments}|Snakes], Segments, SkipHeadPid) ->
	get_all_segments(Snakes, CurrentSegments ++ Segments, SkipHeadPid).

%% Collision for a snake is when its head shares X,Y with some other segment
%% Heads of two snakes colliding will result in two collisions
	
move_segments(NewX, NewY, Segments, ShouldGrow) ->
	move_segments(NewX, NewY, Segments, [], ShouldGrow).
	
move_segments(_NewX, _NewY, [], MovedSegments, _ShouldGrow) ->
	lists:reverse(MovedSegments);

move_segments(NewX, NewY, Segments, _MovedSegments, true) ->	
	[#segment{x = NewX, y = NewY}|Segments];

%% X,Y of the preceding segment becomes X,Y for the current segment
move_segments(NewX, NewY, [_Segment = #segment{x = NextX, y = NextY}|Segments], MovedSegments, false) ->
	move_segments(NextX, NextY, Segments, [#segment{x = NewX, y = NewY}|MovedSegments], false).

get_free_random_spot(#game{size_x = SizeX, size_y = SizeY, snakes = Snakes, treasures = Treasures}) ->
	OccupiedSpots = extract_segment_positions(Snakes) ++ [{X, Y} || #treasure{x = X, y = Y} <- Treasures],
	
	get_free_random_spot(OccupiedSpots, SizeX, SizeY).

get_free_random_spot(OccupiedSpots, SizeX, SizeY) ->
	Spot = {rand:uniform(SizeX) - 1, rand:uniform(SizeY) - 1},
	
	case lists:member(Spot, OccupiedSpots) of
		true -> get_free_random_spot(OccupiedSpots, SizeX, SizeY);
		false -> Spot
	end.

extract_segment_positions(Snakes) ->
	extract_segment_positions(Snakes, []).

extract_segment_positions([], Positions) ->
	Positions;

extract_segment_positions([#snake{segments = Segments}|Snakes], Positions) ->
	extract_segment_positions(Snakes, [{X, Y} || #segment{x = X, y = Y} <- Segments] ++ Positions).
