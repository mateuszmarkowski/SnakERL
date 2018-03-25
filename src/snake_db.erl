-module(snake_db).

-define(GAME_TABLE, snake_game_table).

-export([start/0, update_game/2, new_game/2, list_games/1, get_game/2, delete_game/2, server/0]).

-include("records.hrl").

start() ->
	case ets:info(?GAME_TABLE, owner) of
		undefined ->
			ets:new(?GAME_TABLE, [set, named_table, {keypos, #game.pid}]),
			ServerPid = spawn(?MODULE, server, []),
			ets:give_away(?GAME_TABLE, ServerPid, []),
			ServerPid;
		ServerPid -> ServerPid
	end.
	
server() ->
	receive
		{update_game, Game} -> 
			ets:update_element(?GAME_TABLE, Game#game.pid, [{#game.snakes, Game#game.snakes}, {#game.treasures, Game#game.treasures}, {#game.state, Game#game.state}]), server();
		{new_game, Game} ->
			ets:insert(?GAME_TABLE, Game), server();
		{list_games, Pid} ->
			Pid ! {list_games_response, ets:tab2list(?GAME_TABLE)}, server();
		{get_game, GamePid, Pid} ->
			Pid ! {get_game_response, lists:nth(1, ets:lookup(?GAME_TABLE, GamePid))}, server();
		{delete_game, Game} ->
			ets:delete(?GAME_TABLE, Game#game.pid),
			server()
	end.

update_game(ServerPid, Game) ->
	ServerPid ! {update_game, Game}.
	
new_game(ServerPid, Game) ->
	ServerPid ! {new_game, Game}.
	
list_games(ServerPid) ->
	ServerPid ! {list_games, self()},
	receive 
		{list_games_response, Games} -> Games;
		_ -> []
	end.

get_game(ServerPid, GamePid) ->
	ServerPid ! {get_game, GamePid, self()},
	
	receive
		{get_game_response, Game} -> Game
	end.

delete_game(ServerPid, Game) ->
	ServerPid ! {delete_game, Game}.
