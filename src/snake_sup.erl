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
    {ok, { {one_for_one, 5, 10}, [
	    {snake_system, {snake_system, start_link, []}, permanent, brutal_kill, worker, []},
	    {snake_db, {snake_db, start_link, []}, permanent, brutal_kill, worker, []}
    ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
