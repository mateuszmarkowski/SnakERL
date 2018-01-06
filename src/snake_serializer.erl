-module(snake_serializer).

-export([text_to_term/1, term_to_text/1]).

-include("records.hrl").

%%
%% J#<0.23.2> - join game {join, <0.23.2>}
%% S#10,20 - start a new game {start, 10, 20}
%% U#<0.23.2>=14,5;15,6;16,6|<0.55.1>=0,1;1,2;1,3 - game update #game{...}
%% D#1 - update direction {direction, 1}
%% L# - list games
%% L#<0.23.2>;<0.23.1> - response to list of games

text_to_term(Text) ->
	case string:slice(Text, 0, 2) of
		<<"J#">> -> parse_join(Text);
		<<"S#">> -> parse_start(Text);
		<<"D#">> -> parse_direction(Text);
		<<"L#">> -> list;
		_ -> invalid
	end.

parse_join(Text) ->
	{join, list_to_pid(binary_to_list(string:slice(Text, 2)))}.

parse_start(Text) ->
	[SizeX, SizeY] = string:split(string:slice(Text, 2), ",", all),
	{start, list_to_integer(binary_to_list(SizeX)), list_to_integer(binary_to_list(SizeY))}.

parse_direction(Text) ->
	{direction, list_to_integer(binary_to_list(string:slice(Text, 2)))}.

%% #game{snakes=Snakes}
term_to_text({update, #game{snakes = Snakes}}) ->
	"D#" ++ snakes_to_text(Snakes);

term_to_text({list, Games}) ->
	"L#" ++ string:join([pid_to_list(Pid) || #game{pid = Pid} <- Games], ";").

snakes_to_text(Snakes) ->
	snakes_to_text(Snakes, []).

snakes_to_text([], Parts) ->
	string:join(lists:reverse(Parts), "|");

snakes_to_text([#snake{pid = Pid, segments = Segments}|Snakes], Parts) ->
	snakes_to_text(
		Snakes,
		[pid_to_list(Pid) ++ "=" ++ string:join([integer_to_list(X) ++ "," ++ integer_to_list(Y) || #segment{x = X, y = Y} <- Segments], ";")|Parts]
	).
