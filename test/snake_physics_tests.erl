-module(snake_physics_tests).

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
				false,
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
				#snake{direction = 3, segments = [#segment{x=0, y=1}, #segment{x=0, y=0}, #segment{x=1, y=0}]},
				10,
				10
			)
		}
	].
	
detect_collisions_test_() ->
	[
		{
			"No collision.",
			test_detect_collisions(
				#game{
					snakes = [
						#snake{pid=list_to_pid("<0.1.0>"), segments = [#segment{x=10, y=11}, #segment{x=10, y=12}, #segment{x=10, y=13}]},
						#snake{pid=list_to_pid("<0.2.0>"), segments = [#segment{x=2, y=11}, #segment{x=2, y=12}, #segment{x=2, y=13}]}
					]
				},
				[
						#snake{pid=list_to_pid("<0.2.0>"), state = active, segments = [#segment{x=2, y=11}, #segment{x=2, y=12}, #segment{x=2, y=13}]},
						#snake{pid=list_to_pid("<0.1.0>"), state = active, segments = [#segment{x=10, y=11}, #segment{x=10, y=12}, #segment{x=10, y=13}]}
				]
			)
		},
		{
			"A simple collision.",
			test_detect_collisions(
				#game{
					snakes = [
						#snake{pid=list_to_pid("<0.1.0>"), segments = [#segment{x=2, y=1}, #segment{x=2, y=2}, #segment{x=2, y=3}]},
						#snake{pid=list_to_pid("<0.2.0>"), segments = [#segment{x=2, y=2}, #segment{x=3, y=2}, #segment{x=4, y=2}]}
					]
				},
				[
					#snake{pid=list_to_pid("<0.2.0>"), state = collision, segments = [#segment{x=2, y=2}, #segment{x=3, y=2}, #segment{x=4, y=2}]},
					#snake{pid=list_to_pid("<0.1.0>"), state = active, segments = [#segment{x=2, y=1}, #segment{x=2, y=2}, #segment{x=2, y=3}]}
				]
			)
		},
		{
			"A double collision.",
			test_detect_collisions(
				#game{
					snakes = [
						#snake{pid=list_to_pid("<0.1.0>"), segments = [#segment{x=2, y=1}, #segment{x=2, y=2}, #segment{x=2, y=3}]},
						#snake{pid=list_to_pid("<0.2.0>"), segments = [#segment{x=2, y=1}, #segment{x=3, y=2}, #segment{x=4, y=3}]}
					]
				},
				[
					#snake{pid=list_to_pid("<0.2.0>"), state = collision, segments = [#segment{x=2, y=1}, #segment{x=3, y=2}, #segment{x=4, y=3}]},
					#snake{pid=list_to_pid("<0.1.0>"), state = collision, segments = [#segment{x=2, y=1}, #segment{x=2, y=2}, #segment{x=2, y=3}]}
				]
			)
		}
	].

test_move_segments(NewX, NewY, Segments, ShouldGrow, ExpectedSegments) ->
	?_assertEqual(ExpectedSegments, snake_physics:move_segments(NewX, NewY, Segments, ShouldGrow)).

test_move_snake(Snake, ExpectedSnake, SizeX, SizeY) ->
	?_assertEqual(ExpectedSnake, snake_physics:move_snake(Snake, SizeX, SizeY)).

test_detect_collisions(Game, ExpectedSnakes) ->
	?_assertEqual(ExpectedSnakes, snake_physics:detect_collisions(Game)).
	
	