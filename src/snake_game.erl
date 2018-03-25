-module(snake_game).

-include("records.hrl").

-export([start/4, server/0, join/3, leave/2, direction/3]).

start(X, Y, Name, MaxSnakes) ->
	GamePid = spawn(?MODULE, server, []),
	snake_db:new_game(#game{pid = GamePid, size_x = X, size_y = Y, name = Name, max_snakes = MaxSnakes}),
	timer:send_interval(300, GamePid, tick),
	GamePid.

join(GamePid, ClientPid, Name) ->
	GamePid ! {join, ClientPid, Name},
	ok.

leave(GamePid, ClientPid) ->
	GamePid ! {leave, ClientPid},
	ok.

direction(GamePid, ClientPid, Direction) ->
	GamePid ! {direction, ClientPid, Direction},
	ok.

server() ->
	receive
		{join, ClientPid, Name} ->
			Game = snake_db:get_game(self()),
			Game2 = Game#game{snakes = [#snake{pid = ClientPid, name = Name, direction = 3, segments = [#segment{x = 0, y = 0}]}|Game#game.snakes]},
			snake_db:update_game(Game2),
			ClientPid ! {id, ClientPid},
			broadcast_game_list(),
			server();
		{leave, ClientPid} ->
			%%lager:info("Leaving PID: ~p Game PID: ~p", [ClientPid, self()]),
			Game = snake_db:get_game(self()),
			Game2 = Game#game{snakes = [Snake || Snake = #snake{pid = Pid} <- Game#game.snakes, Pid =/= ClientPid]},
			
			case length(Game2#game.snakes) of
				0 ->
					lager:info("Game ~p stopped because there are no more snakes.", [self()]),
					snake_db:delete_game(Game2),
					broadcast_game_list();
				_ -> 
					snake_db:update_game(Game2),
					broadcast_game_list(),
					server()
			end;

		{direction, ClientPid, Direction} ->
			Game = snake_db:get_game(self()),
			Game2 = Game#game{snakes = lists:map(
				fun (Snake = #snake{pid = CurrentClientPid}) ->

					case CurrentClientPid of
						
						ClientPid -> snake_physics:maybe_change_direction(Game, Snake, Direction);
						_ -> Snake
					end
				end,
				Game#game.snakes)},
			snake_db:update_game(Game2),
			server();
		tick ->
			%%lager:info("Game ~p has been ticked.", [self()]),
			Game = snake_db:get_game(self()),
			%% Strip snakes whose collision was detected during the previous tick
			Game2 = Game#game{snakes = snake_physics:strip_colliding_snakes(Game#game.snakes)},
			Game3 = snake_physics:detect_consumptions(Game2),
			Game4 = snake_physics:move_snakes(Game3),
			%% Update state of colliding snakes and give the interface a chance to tell user about it
			Game5 = Game4#game{snakes = snake_physics:detect_collisions(Game4)},
			
			Game6 = case rand:uniform(10) of
				1 -> snake_physics:add_treasure(Game5, fodder);
				_ -> Game5
			end,
			%%lager:info("GAME ~p", [Game6]),
			snake_db:update_game(Game6),
			broadcast_updates(Game6),
			server();
		_ ->
			ok, server()
	end.

broadcast_game_list() ->
	snake_system:broadcast(snake_system, {list, snake_db:list_games()}).

broadcast_updates(Game = #game{snakes = Snakes}) ->
	broadcast_updates(Game, [ClientPid || Snake = #snake{pid = ClientPid} <- Snakes]).

broadcast_updates(_Game, []) ->
	ok;

broadcast_updates(Game, [ClientPid|ClientPids]) ->
	ClientPid ! {update, Game},
	broadcast_updates(Game, ClientPids).

