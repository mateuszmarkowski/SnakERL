%%
%% Module responsible for game logic. A new game server is spun for every new game. Players
%% joining existing games connect to existing game servers. Game server is shutdown when
%% all players leave.
%%
-module(snake_game).

-behaviour(gen_server).

-include("records.hrl").

-export([start_link/4, init/1, join/3, leave/2, direction/3, handle_call/3, handle_cast/2, handle_info/2]).

start_link(X, Y, Name, MaxSnakes) ->
	%% We have to use PID as we'll have multiple Games and we don't need a unique name for each
	gen_server:start_link(?MODULE, [X, Y, Name, MaxSnakes], []).

init([X, Y, Name, MaxSnakes]) ->
	snake_db:new_game(#game{pid = self(), size_x = X, size_y = Y, name = Name, max_snakes = MaxSnakes}),
	timer:send_interval(300, self(), tick),
	{ok, []}.

join(GamePid, ClientPid, Name) ->
	gen_server:cast(GamePid, {join, ClientPid, Name}).

leave(GamePid, ClientPid) ->
	gen_server:cast(GamePid, {leave, ClientPid}).

direction(GamePid, ClientPid, Direction) ->
	gen_server:cast(GamePid, {direction, ClientPid, Direction}).

handle_cast({join, ClientPid, Name}, State) ->
	Game = snake_db:get_game(self()),
	Game2 = Game#game{snakes = [#snake{pid = ClientPid, name = Name, direction = 3, segments = [#segment{x = 0, y = 0}]}|Game#game.snakes]},
	snake_db:update_game(Game2),
	ClientPid ! {id, ClientPid},
	broadcast_game_list(),
	{noreply, State};
	
handle_cast({leave, ClientPid}, State) ->
	%%lager:info("Leaving PID: ~p Game PID: ~p", [ClientPid, self()]),
	Game = snake_db:get_game(self()),
	Game2 = Game#game{snakes = [Snake || Snake = #snake{pid = Pid} <- Game#game.snakes, Pid =/= ClientPid]},
	
	case length(Game2#game.snakes) of
		0 ->
			lager:info("Game ~p stopped because there are no more snakes.", [self()]),
			snake_db:delete_game(Game2),
			broadcast_game_list(),
			{stop, normal, State};
		_ -> 
			snake_db:update_game(Game2),
			broadcast_game_list(),
			{noreply, State}
	end;

handle_cast({direction, ClientPid, Direction}, State) ->
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
	{noreply, State};

handle_cast(Request, State) ->
	lager:error("Unexpected cast received: ~p.", [Request]),
	{noreply, State}.

handle_call(Request, From, State) ->
	lager:error("Unexpected call received from ~p: ~p.", [From, Request]).

handle_info(tick, State) ->
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
	{noreply, State}.

broadcast_game_list() ->
	snake_system:broadcast(snake_system, {list, snake_db:list_games()}).

broadcast_updates(Game = #game{snakes = Snakes}) ->
	broadcast_updates(Game, [ClientPid || Snake = #snake{pid = ClientPid} <- Snakes]).

broadcast_updates(_Game, []) ->
	ok;

broadcast_updates(Game, [ClientPid|ClientPids]) ->
	ClientPid ! {update, Game},
	broadcast_updates(Game, ClientPids).

