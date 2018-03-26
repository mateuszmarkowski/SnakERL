%%
%% The only purpose of this module is collecting PIDs of all connected users and making
%% it possible to notify all about some changes. It is important that connected users may
%% include PIDs which aren't participating in any game, yet we need to be able to send
%% updates to these PIDs. E.g. a list of games may change and we want to notify all
%% connected users about.
%%
-module(snake_system).

-behaviour(gen_server).

-export([handle_call/3, handle_cast/2, init/1, start_link/0, broadcast/2, join/2]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	{ok, []}.

broadcast(Server, Message) ->
	gen_server:cast(Server, {broadcast, Message}).

join(Server, ClientPid) ->
	gen_server:cast(Server, {join, ClientPid}).

handle_call(Request, From, ClientPids) ->
	{noreply, ok, ClientPids}.
	
handle_cast({join, ClientPid}, ClientPids) ->
	{noreply, [ClientPid|ClientPids]};
	
handle_cast({broadcast, Message}, ClientPids) ->

	send_message(ClientPids, Message),
	{noreply, ClientPids};

handle_cast(_Message, ClientPids) ->
	{noreply, ClientPids}.
	
send_message([], Message) ->
	ok;
	
send_message([ClientPid|ClientPids], Message) ->
	ClientPid ! Message,
	send_message(ClientPids, Message).
