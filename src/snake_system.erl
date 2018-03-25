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
