%%%-------------------------------------------------------------------
%% @doc snake public API
%% @end
%%%-------------------------------------------------------------------

-module(snake_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, snake, "/index.html"}},
			{"/websocket", snake_handler, []}
		]}
	]),
	lager:info("started snake..."),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),
	lager:info("started http..."),
    snake_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
