-module(snake_serializer_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

term_to_text_test_() ->
	[
		{
			"Update about a single snake game.",
			test_term_to_text(
				{update, #game{snakes = [
					#snake{pid = list_to_pid("<0.23.2>"), segments = [
						#segment{x = 10, y = -3},
						#segment{x = 11, y = -3},
						#segment{x = 12, y = -3},
						#segment{x = 12, y = -4}
					]}
				]}},
				"D#<0.23.2>=10,-3;11,-3;12,-3;12,-4"
			)
		},
		{
			"Update about a two-snakes game.",
			test_term_to_text(
				{update, #game{snakes = [
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
				"D#<0.23.2>=10,-3;11,-3;12,-3;12,-4|<0.41.1>=6,10;7,11;8,12;9,13"
			)
		},
		{
			"List of two games.",
			test_term_to_text(
				{list, [
					#game{pid = list_to_pid("<0.23.1>")},
					#game{pid = list_to_pid("<0.100.2>")}
				]},
				"L#<0.23.1>;<0.100.2>"
			)
		}
	].
	
text_to_term_test_() ->
	[
		{
			"Joining a game.",
			test_text_to_term(
				<<"J#<0.12.2>">>,
				{join, list_to_pid("<0.12.2>")}
			)
		},
		{
			"Starting a new game.",
			test_text_to_term(
				<<"S#40,20">>,
				{start, 40, 20}
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
	