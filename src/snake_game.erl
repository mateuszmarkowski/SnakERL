-module(snake_game).

-define(DIRECTION_UP, 1).
-define(DIRECTION_RIGHT, 2).
-define(DIRECTION_DOWN, 3).
-define(DIRECTION_LEFT, 4).

-include("records.hrl").

-export([start/3, server/1, join/2, leave/2, direction/3]).

-ifdef(EUNIT).
-export([move_segments/4, move_snake/1]).
-endif.

start(DbPid, X, Y) ->
	GamePid = spawn(?MODULE, server, [DbPid]),
	snake_db:new_game(DbPid, #game{pid = GamePid, size_x = X, size_y = Y}),
	timer:send_interval(1000, GamePid, tick),
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
						
						ClientPid -> maybe_change_direction(Snake, Direction);
						_ -> Snake
					end
				end,
				Game#game.snakes)},
			snake_db:update_game(DbPid, Game2),
			server(DbPid);
		tick ->
			lager:info("Game ~p has been ticked.", [self()]),
			Game = snake_db:get_game(DbPid, self()),
			Game2 = move_snakes(Game),
			snake_db:update_game(DbPid, Game2),
			broadcast_updates(Game2),
			server(DbPid);
		_ ->
			ok, server(DbPid)
	end.

%% Snakes can't move backwards!
maybe_change_direction(Snake = #snake{segments = Segments}, Direction) when length(Segments) > 1 ->
	%% The first segment is the head of our snake.
	Segment1 = lists:nth(1, Segments),
	Segment2 = lists:nth(2, Segments),
	
	{NewX, NewY} = calculate_new_x_y(Segment1#segment.x, Segment1#segment.y, Direction),
	
	Collides = (Segment2#segment.x =:= NewX) and (Segment2#segment.y =:= NewY),
	
	case Collides of
		true -> Snake;
		%% Snakes grow when changing direction, just for testing.
		_ -> Snake#snake{direction = Direction, growth = 1}
	end;
	
maybe_change_direction(Snake, Direction) ->
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

move_snakes(Game, [Snake|Snakes], MovedSnakes) ->
	move_snakes(Game, Snakes, [move_snake(Snake)|MovedSnakes]).

move_snake(Snake = #snake{growth = Growth, direction = Direction, segments = [Segment = #segment{x = X, y = Y}|Segments]}) ->
	ShouldGrow = Growth > 0,
	
	%% Snakes grow one segment per tick as long as there're pending segments (Growth - number of pending segments)
	Growth2 = case Growth of
		G when G > 1 -> G - 1;
		_ -> 0
	end,
	
	{NewX, NewY} = calculate_new_x_y(X, Y, Direction),
	
	Segments2 = move_segments(NewX, NewY, [Segment] ++ Segments, ShouldGrow),
	
	Snake#snake{segments = Segments2, growth = Growth2}.

calculate_new_x_y(X, Y, Direction) when Direction =:= ?DIRECTION_UP    -> {X, Y - 1};

calculate_new_x_y(X, Y, Direction) when Direction =:= ?DIRECTION_RIGHT -> {X + 1, Y};

calculate_new_x_y(X, Y, Direction) when Direction =:= ?DIRECTION_DOWN  -> {X, Y + 1};

calculate_new_x_y(X, Y, Direction) when Direction =:= ?DIRECTION_LEFT  -> {X - 1, Y}.

check_collision(_Game, X, Y) when X < 0; Y < 0 -> border;

check_collision(Game = #game{size_x = SizeX, size_y = SizeY}, X, Y) when X >= SizeX; Y >= SizeY -> border;
	
check_collision(Game = #game{snakes = Snakes}, X, Y) ->
	%% Extract segments from all snakes.
	case check_collision_with_segments(lists:foldl(fun(S = #snake{segments = Segments}, AllSegments) -> Segments ++ AllSegments end, [], Snakes), X, Y) of
		true -> segment;
		false -> ok
	end.

check_collision_with_segments([], X, Y) -> false;

check_collision_with_segments([#segment{x = SX, y = SY}|Segments], X, Y) when SX == X; SY == Y -> true;

check_collision_with_segments([_Segment|Segments], X, Y) ->
	check_collision_with_segments(Segments, X, Y).
	
move_segments(NewX, NewY, Segments, ShouldGrow) ->
	move_segments(NewX, NewY, Segments, [], ShouldGrow).
	
move_segments(NewX, NewY, [], MovedSegments, _ShouldGrow) ->
	lists:reverse(MovedSegments);

move_segments(NewX, NewY, Segments, _MovedSegments, true) ->	
	[#segment{x = NewX, y = NewY}|Segments];

%% X,Y of the preceding segment becomes X,Y for the current segment
move_segments(NewX, NewY, [Segment = #segment{x = NextX, y = NextY}|Segments], MovedSegments, false) ->
	move_segments(NextX, NextY, Segments, [#segment{x = NewX, y = NewY}|MovedSegments], false).
