%%%-------------------------------------------------------------------
%% @doc snake top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(snake_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, snake, "/index.html"}},
			{"/style.css", cowboy_static, {priv_file, snake, "/style.css"}},
			{"/game.js", cowboy_static, {priv_file, snake, "/game.js"}},
			{"/websocket", snake_handler, []}
		]}
	]),
	
    {ok, { {one_for_one, 5, 10}, [
	    {snake_cowboy, {cowboy, start_http, [http, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]]}, permanent, brutal_kill, worker, []},
	    {snake_system, {snake_system, start_link, []}, permanent, brutal_kill, worker, []},
	    {snake_db, {snake_db, start_link, []}, permanent, brutal_kill, worker, []}
    ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
