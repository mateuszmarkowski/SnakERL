-module(snake_db).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, update_game/1, new_game/1, list_games/0, get_game/1, delete_game/1]).

-include("records.hrl").

-define(GAME_TABLE, snake_game_table).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
	
init([]) ->
	case ets:info(?GAME_TABLE, owner) of
		undefined -> ets:new(?GAME_TABLE, [set, named_table, {keypos, #game.pid}]);
		ServerPid -> ServerPid
	end,
	{ok, []}.
	
update_game(Game) ->
	gen_server:cast(?MODULE, {update_game, Game}).
	
new_game(Game) ->
	gen_server:cast(?MODULE, {new_game, Game}).
	
list_games() ->
	gen_server:call(?MODULE, list_games).

get_game(GamePid) ->
	gen_server:call(?MODULE, {get_game, GamePid}).

delete_game(Game) ->
	gen_server:cast(?MODULE, {delete_game, Game}).

handle_cast({update_game, Game}, State) ->
	ets:update_element(?GAME_TABLE, Game#game.pid, [{#game.snakes, Game#game.snakes}, {#game.treasures, Game#game.treasures}, {#game.state, Game#game.state}]),
	{noreply, State};
	
handle_cast({new_game, Game}, State) ->
	ets:insert(?GAME_TABLE, Game), server,
	{noreply, State};
	
handle_cast({delete_game, Game}, State) ->
	ets:delete(?GAME_TABLE, Game#game.pid),
	{noreply, State}.
	
handle_call(list_games, _From, State) ->
	{reply, ets:tab2list(?GAME_TABLE), State};

handle_call({get_game, GamePid}, _From, State) ->
	{reply, lists:nth(1, ets:lookup(?GAME_TABLE, GamePid)), State};

handle_call(_Req, _From, State) ->
	{reply, {error, badarg}, State}.
	
