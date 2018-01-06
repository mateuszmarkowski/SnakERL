-module(snake_game_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

move_segments_test_() ->
	[
		
		{
			"Move snake.",
			test_move_segments(
				0,
				1,
				[#segment{x=0, y=0}, #segment{x=1, y=0}, #segment{x=2, y=0}],
				[#segment{x=0, y=1}, #segment{x=0, y=0}, #segment{x=1, y=0}]
			)
		}
	].

move_snake_test_() ->
	[
		{
			"Move down.",
			test_move_snake(
				#snake{direction = 3, segments = [#segment{x=0, y=0}, #segment{x=1, y=0}, #segment{x=2, y=0}]},
				#snake{direction = 3, segments = [#segment{x=0, y=1}, #segment{x=0, y=0}, #segment{x=1, y=0}]}
			)
		}
	].

test_move_segments(NewX, NewY, Segments, ExpectedSegments) ->
	?_assertEqual(ExpectedSegments, snake_game:move_segments(NewX, NewY, Segments)).

test_move_snake(Snake, ExpectedSnake) ->
	?_assertEqual(ExpectedSnake, snake_game:move_snake(Snake)).
