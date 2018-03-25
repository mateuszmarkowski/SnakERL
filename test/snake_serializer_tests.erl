-module(snake_serializer_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

term_to_text_test_() ->
	[
		{
			"Update about a single snake game.",
			test_term_to_text(
				{update, #game{state = pending, snakes = [
					#snake{pid = list_to_pid("<0.23.2>"), segments = [
						#segment{x = 10, y = -3},
						#segment{x = 11, y = -3},
						#segment{x = 12, y = -3},
						#segment{x = 12, y = -4}
					]}
				]}},
				"D#P__<0.23.2>=A=10,-3;11,-3;12,-3;12,-4"
			)
		},
		{
			"Update about a two-snakes game.",
			test_term_to_text(
				{update, #game{state = pending, snakes = [
					#snake{pid = list_to_pid("<0.23.2>"), segments = [
						#segment{x = 10, y = -3},
						#segment{x = 11, y = -3},
						#segment{x = 12, y = -3},
						#segment{x = 12, y = -4}
					]},
					#snake{pid = list_to_pid("<0.41.1>"), segments = [
						#segment{x = 6, y = 10},
						#segment{x = 7, y = 11},
						#segment{x = 8, y = 12},
						#segment{x = 9, y = 13}
					]}
				]}},
				"D#P__<0.23.2>=A=10,-3;11,-3;12,-3;12,-4|<0.41.1>=A=6,10;7,11;8,12;9,13"
			)
		},
		{
			"List of two games.",
			test_term_to_text(
				{list, [
					#game{pid = list_to_pid("<0.23.1>"), name = <<"Game1">>, max_snakes = 2, snakes = []},
					#game{pid = list_to_pid("<0.100.2>"), name = <<"Game2">>, max_snakes = 2, snakes = [#snake{}]}
				]},
				"L#<0.23.1>,Game1,0,2;<0.100.2>,Game2,1,2"
			)
		}
	].
	
text_to_term_test_() ->
	[
		{
			"Joining a game.",
			test_text_to_term(
				<<"J#<0.12.2>,Player1">>,
				{join, list_to_pid("<0.12.2>"), <<"Player1">>}
			)
		},
		{
			"Starting a new game.",
			test_text_to_term(
				<<"S#40,20,SuperGame,2">>,
				{start, 40, 20, <<"SuperGame">>, 2}
			)
		},
		{
			"Changin direction.",
			test_text_to_term(
				<<"D#3">>,
				{direction, 3}
			)
		}
	].
	
test_term_to_text(Term, ExpectedText) ->
	?_assertEqual(ExpectedText, snake_serializer:term_to_text(Term)).
	
test_text_to_term(Text, ExpectedTerm) ->
	?_assertEqual(ExpectedTerm, snake_serializer:text_to_term(Text)).
	