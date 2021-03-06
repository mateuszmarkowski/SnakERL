%%
%% Module responsible for deserializing messages received from the web client. The protocol
%% is very simple and consists of just a few message types indicated by the first character
%% of the message. The first character is followed by # and additional details.
%%
%% J#<0.23.2> - join game {join, <0.23.2>}
%% S#10,20 - start a new game {start, 10, 20}
%% U#<0.23.2>=14,5;15,6;16,6|<0.55.1>=0,1;1,2;1,3 - game update #game{...}
%% D#1 - update direction {direction, 1}
%% L# - list games
%% L#<0.23.2>;<0.23.1> - response to list of games

-module(snake_serializer).

-export([text_to_term/1, term_to_text/1]).

-include("records.hrl").

text_to_term(Text) ->
	case string:slice(Text, 0, 2) of
		<<"J#">> -> parse_join(Text);
		<<"S#">> -> parse_start(Text);
		<<"D#">> -> parse_direction(Text);
		<<"L#">> -> list;
		<<"Q#">> -> leave;
		_ -> invalid
	end.

parse_join(Text) ->
	[PidText, Name] = string:split(string:slice(Text, 2), ",", all),
	{join, list_to_pid(binary_to_list(PidText)), Name}.

parse_start(Text) ->
	[SizeX, SizeY, Name, MaxSnakes] = string:split(string:slice(Text, 2), ",", all),
	{start, binary_to_integer(SizeX), binary_to_integer(SizeY), Name, binary_to_integer(MaxSnakes)}.

parse_direction(Text) ->
	{direction, list_to_integer(binary_to_list(string:slice(Text, 2)))}.

term_to_text({id, Pid}) ->
	"I#" ++ pid_to_list(Pid);

%% #game{snakes=Snakes}
term_to_text({update, Game = #game{treasures = Treasures, snakes = Snakes, state = State}}) ->
	"D#" ++ state_to_text(State) ++ "_" ++ treasures_to_text(Treasures) ++ "_" ++ snakes_to_text(Snakes);

term_to_text({list, Games}) ->
	"L#" ++ string:join([pid_to_list(Pid) ++ "," ++ binary_to_list(Name) ++ "," ++ integer_to_list(length(Snakes)) ++ "," ++ integer_to_list(MaxSnakes) || #game{pid = Pid, name = Name, snakes = Snakes, max_snakes = MaxSnakes} <- Games], ";").

snakes_to_text(Snakes) ->
	snakes_to_text(Snakes, []).

snakes_to_text([], Parts) ->
	string:join(lists:reverse(Parts), "|");

snakes_to_text([#snake{pid = Pid, name = Name, state = State, segments = Segments}|Snakes], Parts) ->
	snakes_to_text(
		Snakes,
		[pid_to_list(Pid) ++ "=" ++ binary_to_list(Name) ++ "=" ++ state_to_text(State) ++ "=" ++ string:join([integer_to_list(X) ++ "," ++ integer_to_list(Y) || #segment{x = X, y = Y} <- Segments], ";")|Parts]
	).

treasures_to_text(Treasures) ->
	string:join([treasure_type_to_text(Type) ++ "=" ++ integer_to_list(X) ++ "," ++ integer_to_list(Y) || #treasure{type = Type, x = X, y = Y} <- Treasures], "|").

state_to_text(pending) -> "P";

state_to_text(active) -> "A";

state_to_text(collision) -> "C".

treasure_type_to_text(fodder) -> "F".
