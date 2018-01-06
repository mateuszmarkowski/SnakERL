-module(snake_handler).

-include("records.hrl").

-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3, handle/2, terminate/3]).
-export([websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.
	
handle(_Req, State) ->
	{ok, Req2} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),
	{ok, Req2, State}.
	
websocket_init(_TransportName, Req, _Opts) ->
	lager:info("Socket pid ~p", [self()]),
	lager:info("websocket init"),
	{ok, Req, [{db_pid, snake_db:start()}]}.

websocket_handle({text, Msg}, Req, State) ->
	lager:info("Got message ~p", [Msg]),
	
	lager:info("Parsed: ~p", [snake_serializer:text_to_term(Msg)]),
	
	DbPid = proplists:get_value(db_pid, State),
	
	%% GamePid is available only after user joins a game 
	GamePid = proplists:get_value(game_pid, State),
	
	{Response, State2} = case snake_serializer:text_to_term(Msg) of
		list -> {snake_serializer:term_to_text({list, snake_db:list_games(DbPid)}), State};
		{start, X, Y} -> NewGamePid = snake_game:start(DbPid, X, Y), snake_game:join(NewGamePid, self()), {<<"">>, [{game_pid, NewGamePid}|State]};
		{join, NewGamePid} -> snake_game:join(NewGamePid, self()), {<<"">>, [{game_pid, NewGamePid}|State]};
		{direction, Direction} when is_pid(GamePid) -> snake_game:direction(GamePid, self(), Direction), {<<"">>, State};
		_ -> {<<"fooo">>, State}
	end,

	{reply, {text, Response}, Req, State2, hibernate};	

websocket_handle(_Any, Req, State) ->
	lager:info("Unknown request..."),
	{reply, {text, <<"???">>}, Req, State, hibernate}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
	{reply, {text, Msg}, Req, State};


websocket_info(Update, Req, State) ->
	lager:info("gotten ~p", [Update]),
	{reply, {text, snake_serializer:term_to_text(Update)}, Req, State};

websocket_info(_Info, Req, State) ->
	lager:info("info?"),
	{reply, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, State) ->
	lager:info("I HAVE BEEN TERMINATED ~p", [self()]),
	
	case proplists:get_value(game_pid, State) of
		undefined -> ok;
		GamePid -> snake_game:leave(GamePid, self())
	end,
	ok.

terminate(_Reason, _Req, _State) ->
	ok.
