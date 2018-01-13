-module(snake_game).

-define(DIRECTION_UP, 1).
-define(DIRECTION_RIGHT, 2).
-define(DIRECTION_DOWN, 3).
-define(DIRECTION_LEFT, 4).

-include("records.hrl").

-export([start/3, server/1, join/2, leave/2, direction/3]).

-ifdef(EUNIT).
-export([move_segments/4, move_snake/1, detect_collisions/1]).
-endif.

start(DbPid, X, Y) ->
	GamePid = spawn(?MODULE, server, [DbPid]),
	snake_db:new_game(DbPid, #game{pid = GamePid, size_x = X, size_y = Y}),
	timer:send_interval(500, GamePid, tick),
	GamePid.

join(GamePid, ClientPid) ->
	GamePid ! {join, ClientPid},
	ok.

leave(GamePid, ClientPid) ->
	GamePid ! {leave, ClientPid},
	ok.

direction(GamePid, ClientPid, Direction) ->
	GamePid ! {direction, ClientPid, Direction},
	ok.

server(DbPid) ->
	receive
		{join, ClientPid} ->
			Game = snake_db:get_game(DbPid, self()),
			Game2 = Game#game{snakes = [#snake{pid = ClientPid, direction = 3, segments = [#segment{x = 0, y = 0}]}|Game#game.snakes]},
			snake_db:update_game(DbPid, Game2),
			ClientPid ! {id, ClientPid},
			server(DbPid);
		{leave, ClientPid} ->
			Game = snake_db:get_game(DbPid, self()),
			Game2 = Game#game{snakes = [Snake || Snake = #snake{pid = Pid} <- Game#game.snakes, Pid =/= ClientPid]},
			
			case length(Game2#game.snakes) of
				0 -> lager:info("Game ~p stopped because there are no more snakes.", [self()]), snake_db:delete_game(DbPid, Game2);
				_ -> 
					snake_db:update_game(DbPid, Game2),
					server(DbPid)
			end;

		{direction, ClientPid, Direction} ->
			Game = snake_db:get_game(DbPid, self()),
			Game2 = Game#game{snakes = lists:map(
				fun (Snake = #snake{pid = CurrentClientPid}) ->
					lager:info("???"),
					case CurrentClientPid of
						
						ClientPid -> maybe_change_direction(Game, Snake, Direction);
						_ -> Snake
					end
				end,
				Game#game.snakes)},
			snake_db:update_game(DbPid, Game2),
			server(DbPid);
		tick ->
			lager:info("Game ~p has been ticked.", [self()]),
			Game = snake_db:get_game(DbPid, self()),
			%% Strip snakes whose collision was detected during the previous tick
			Game2 = Game#game{snakes = strip_colliding_snakes(Game#game.snakes)},
			Game3 = move_snakes(Game2),
			%% Update state of colliding snakes and give the interface a chance to tell user about it
			Game4 = Game3#game{snakes = detect_collisions(Game3)},
			snake_db:update_game(DbPid, Game4),
			broadcast_updates(Game4),
			server(DbPid);
		_ ->
			ok, server(DbPid)
	end.

strip_colliding_snakes(Snakes) ->	
	[Snake || Snake = #snake{state = State} <- Snakes, State =/= collision].

%% Snakes can't move backwards!
maybe_change_direction(Game = #game{size_x = SizeX, size_y = SizeY}, Snake = #snake{segments = Segments}, Direction) when length(Segments) > 1 ->
	%% The first segment is the head of our snake.
	Segment1 = lists:nth(1, Segments),
	Segment2 = lists:nth(2, Segments),
	
	{NewX, NewY} = calculate_new_x_y(Segment1#segment.x, Segment1#segment.y, Direction, SizeX, SizeY),
	
	Collides = (Segment2#segment.x =:= NewX) and (Segment2#segment.y =:= NewY),
	
	case Collides of
		true -> Snake;
		%% Snakes grow when changing direction, just for testing.
		_ -> Snake#snake{direction = Direction, growth = 1}
	end;
	
maybe_change_direction(_Game, Snake, Direction) ->
	Snake#snake{direction = Direction, growth = 1}.

broadcast_updates(Game = #game{snakes = Snakes}) ->
	broadcast_updates(Game, [ClientPid || Snake = #snake{pid = ClientPid} <- Snakes]).

broadcast_updates(_Game, []) ->
	ok;

broadcast_updates(Game, [ClientPid|ClientPids]) ->
	ClientPid ! {update, Game},
	broadcast_updates(Game, ClientPids).

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

detect_collisions(Game = #game{snakes = Snakes}) ->
	detect_collisions(Snakes, Snakes, []).
	
detect_collisions([], _AllSnakes, AnalyzedSnakes) ->
	AnalyzedSnakes;

detect_collisions([Snake = #snake{pid = Pid, segments = [Segment|Segments]}|RemainingSnakes], AllSnakes, AnalyzedSnakes) ->
	case does_collide(Segment, get_all_segments(AllSnakes, Pid)) of	
		true -> detect_collisions(RemainingSnakes, AllSnakes, [Snake#snake{state = collision}|AnalyzedSnakes]);
		false -> detect_collisions(RemainingSnakes, AllSnakes, [Snake|AnalyzedSnakes])
	end.

does_collide(_HeadSegment, []) -> false;

does_collide(HeadSegment = #segment{x = HX, y = HY}, [Segment = #segment{x = X, y = Y}|Segments]) when HX == X, HY == Y -> true;

does_collide(HeadSegment, [_Segment|Segments]) ->
	does_collide(HeadSegment, Segments).

%% Get a list of all occupied pixels except for the pixel occupied by the head of the snake identified by SkipHeadPid.
get_all_segments(AllSnakes, SkipHeadPid) when is_pid(SkipHeadPid) ->
	get_all_segments(AllSnakes, [], SkipHeadPid).

get_all_segments([], Segments, _SkipHeadPid) ->
	Segments;

get_all_segments([Snake = #snake{pid = Pid, segments = [HeadSegment|CurrentSegments]}|Snakes], Segments, SkipHeadPid) when Pid == SkipHeadPid ->
	get_all_segments(Snakes, CurrentSegments ++ Segments, SkipHeadPid);

get_all_segments([Snake = #snake{pid = Pid, segments = CurrentSegments}|Snakes], Segments, SkipHeadPid) ->
	get_all_segments(Snakes, CurrentSegments ++ Segments, SkipHeadPid).

%% Collision for a snake is when its head shares X,Y with some other segment
%% Heads of two snakes colliding will result in two collisions
	
move_segments(NewX, NewY, Segments, ShouldGrow) ->
	move_segments(NewX, NewY, Segments, [], ShouldGrow).
	
move_segments(NewX, NewY, [], MovedSegments, _ShouldGrow) ->
	lists:reverse(MovedSegments);

move_segments(NewX, NewY, Segments, _MovedSegments, true) ->	
	[#segment{x = NewX, y = NewY}|Segments];

%% X,Y of the preceding segment becomes X,Y for the current segment
move_segments(NewX, NewY, [Segment = #segment{x = NextX, y = NextY}|Segments], MovedSegments, false) ->
	move_segments(NextX, NextY, Segments, [#segment{x = NewX, y = NewY}|MovedSegments], false).
