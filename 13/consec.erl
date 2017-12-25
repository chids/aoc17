-module(consec).
-include_lib("eunit/include/eunit.hrl").
-define(PUZZLE, [{0, 3}, {1, 2}, {2, 6}, {4, 4}, {6, 4}, {8, 8}, {10, 9}, {12, 8}, {14, 5}, {16, 6}, {18, 8}, {20, 6}, {22, 12}, {24, 6}, {26, 12}, {28, 8}, {30, 8}, {32, 10}, {34, 12}, {36, 12}, {38, 8}, {40, 12}, {42, 12}, {44, 14}, {46, 12}, {48, 14}, {50, 12}, {52, 12}, {54, 12}, {56, 10}, {58, 14}, {60, 14}, {62, 14}, {64, 14}, {66, 17}, {68, 14}, {72, 14}, {76, 14}, {80, 14}, {82, 14}, {88, 18}, {92, 14}, {98, 18}]).

first(Layers) -> run(0, Layers, fun(Collision, Collisions) -> [Collision|Collisions] end).

second(Layers) -> brute(0, Layers).

brute(Delay, Layers) ->
  try run(Delay, Layers, fun(_, _) -> throw(collision) end) of
    _ -> Delay
  catch
    collision -> brute(Delay + 1, Layers)
  end.

run(Delay, Layers, Handler) ->
  {Max, _} = hd(lists:reverse(Layers)),
  {Collisions, _} = scan({Delay, Max + 1}, Layers, 0, {[], Handler}),
  lists:sum([ Range * Depth || {Range, Depth} <- Collisions]).

scan(_, [], _, Collisions) -> Collisions;
scan({Tick, Max}, [{Position, Depth}|T], Position, Collisions) ->
  scan({Tick + 1, Max}, T, Position + 1, collision(Tick, Depth, Collisions));
scan({Tick, Max}, [{Layer, Depth}|T], Position, Collisions) ->
  scan({Tick + 1, Max}, [{Layer, Depth}|T], Position + 1, Collisions).

collision(Tick, Depth, {Collisions, Handler}) ->
  case pos(Tick, Depth) of
    0 -> {Handler({Tick, Depth}, Collisions), Handler};
    _ -> {Collisions, Handler}
  end.

pos(Tick, Depth) -> 
  D = Depth - 1,
  case ((Tick div D) band 1) == 0 of
    true -> mod(Tick, D);
    false -> D - mod(Tick, D)
  end.

mod(X, Y) when X > 0 -> X rem Y;
mod(X, Y) when X < 0 -> Y + X rem Y;
mod(0, _) -> 0.
% You need to cross a vast firewall.
% The firewall consists of several layers, each with a security scanner that moves back and forth across the layer.
% To succeed, you must not be detected by a scanner.
% By studying the firewall briefly, you are able to record (in your puzzle input)
% the depth of each layer and the range of the scanning area for the scanner within it, written as depth: range.
% Each layer has a thickness of exactly 1. A layer at depth 0 begins immediately inside the firewall;
% a layer at depth 1 would start immediately after that.
% For example, suppose you've recorded the following:
% 0: 3
% 1: 2
% 4: 4
% 6: 4
% This means that there is a layer immediately inside the firewall (with range 3),
% a second layer immediately after that (with range 2), a third layer which begins at depth 4 (with range 4),
% and a fourth layer which begins at depth 6 (also with range 4). Visually, it might look like this:
%  0   1   2   3   4   5   6
% [ ] [ ] ... ... [ ] ... [ ]
% [ ] [ ]         [ ]     [ ]
% [ ]             [ ]     [ ]
%                 [ ]     [ ]
% Within each layer, a security scanner moves back and forth within its range.
% Each security scanner starts at the top and moves down until it reaches the bottom,
% then moves up until it reaches the top, and repeats. A security scanner takes one picosecond to move one step.
% Drawing scanners as S, the first few picoseconds look like this:
% Picosecond 0:
%  0   1   2   3   4   5   6
% [S] [S] ... ... [S] ... [S]
% [ ] [ ]         [ ]     [ ]
% [ ]             [ ]     [ ]
%                 [ ]     [ ]
% Picosecond 1:
%  0   1   2   3   4   5   6
% [ ] [ ] ... ... [ ] ... [ ]
% [S] [S]         [S]     [S]
% [ ]             [ ]     [ ]
%                 [ ]     [ ]
% Picosecond 2:
%  0   1   2   3   4   5   6
% [ ] [S] ... ... [ ] ... [ ]
% [ ] [ ]         [ ]     [ ]
% [S]             [S]     [S]
%                 [ ]     [ ]
% Picosecond 3:
%  0   1   2   3   4   5   6
% [ ] [ ] ... ... [ ] ... [ ]
% [S] [S]         [ ]     [ ]
% [ ]             [ ]     [ ]
%                 [S]     [S]
% Your plan is to hitch a ride on a packet about to move through the firewall.
% The packet will travel along the top of each layer, and it moves at one layer per picosecond.
% Each picosecond, the packet moves one layer forward (its first move takes it into layer 0),
% and then the scanners move one step. If there is a scanner at the top of the layer as your packet enters it,you are caught.
% (If a scanner moves into the top of its layer while you are there, you are not caught:
% it doesn't have time to notice you before you leave.)
% If you were to do this in the configuration above, marking your current position with parentheses,
% your passage through the firewall would look like this:
% Initial state:
%  0   1   2   3   4   5   6
% [S] [S] ... ... [S] ... [S]
% [ ] [ ]         [ ]     [ ]
% [ ]             [ ]     [ ]
%                 [ ]     [ ]
% Picosecond 0:
%  0   1   2   3   4   5   6
% (S) [S] ... ... [S] ... [S]
% [ ] [ ]         [ ]     [ ]
% [ ]             [ ]     [ ]
%                 [ ]     [ ]
%  0   1   2   3   4   5   6
% ( ) [ ] ... ... [ ] ... [ ]
% [S] [S]         [S]     [S]
% [ ]             [ ]     [ ]
%                 [ ]     [ ]
% Picosecond 1:
%  0   1   2   3   4   5   6
% [ ] ( ) ... ... [ ] ... [ ]
% [S] [S]         [S]     [S]
% [ ]             [ ]     [ ]
%                 [ ]     [ ]
%  0   1   2   3   4   5   6
% [ ] (S) ... ... [ ] ... [ ]
% [ ] [ ]         [ ]     [ ]
% [S]             [S]     [S]
%                 [ ]     [ ]
% Picosecond 2:
%  0   1   2   3   4   5   6
% [ ] [S] (.) ... [ ] ... [ ]
% [ ] [ ]         [ ]     [ ]
% [S]             [S]     [S]
%                 [ ]     [ ]
%  0   1   2   3   4   5   6
% [ ] [ ] (.) ... [ ] ... [ ]
% [S] [S]         [ ]     [ ]
% [ ]             [ ]     [ ]
%                 [S]     [S]
% Picosecond 3:
%  0   1   2   3   4   5   6
% [ ] [ ] ... (.) [ ] ... [ ]
% [S] [S]         [ ]     [ ]
% [ ]             [ ]     [ ]
%                 [S]     [S]
%  0   1   2   3   4   5   6
% [S] [S] ... (.) [ ] ... [ ]
% [ ] [ ]         [ ]     [ ]
% [ ]             [S]     [S]
%                 [ ]     [ ]
% Picosecond 4:
%  0   1   2   3   4   5   6
% [S] [S] ... ... ( ) ... [ ]
% [ ] [ ]         [ ]     [ ]
% [ ]             [S]     [S]
%                 [ ]     [ ]
%  0   1   2   3   4   5   6
% [ ] [ ] ... ... ( ) ... [ ]
% [S] [S]         [S]     [S]
% [ ]             [ ]     [ ]
%                 [ ]     [ ]
% Picosecond 5:
%  0   1   2   3   4   5   6
% [ ] [ ] ... ... [ ] (.) [ ]
% [S] [S]         [S]     [S]
% [ ]             [ ]     [ ]
%                 [ ]     [ ]
%  0   1   2   3   4   5   6
% [ ] [S] ... ... [S] (.) [S]
% [ ] [ ]         [ ]     [ ]
% [S]             [ ]     [ ]
%                 [ ]     [ ]
% Picosecond 6:
%  0   1   2   3   4   5   6
% [ ] [S] ... ... [S] ... (S)
% [ ] [ ]         [ ]     [ ]
% [S]             [ ]     [ ]
%                 [ ]     [ ]
%  0   1   2   3   4   5   6
% [ ] [ ] ... ... [ ] ... ( )
% [S] [S]         [S]     [S]
% [ ]             [ ]     [ ]
%                 [ ]     [ ]
% In this situation, you are caught in layers 0 and 6,
% because your packet entered the layer when its scanner was at the top when you entered it.
% You are not caught in layer 1, since the scanner moved into the top of the layer once you were already there.
% The severity of getting caught on a layer is equal to its depth multiplied by its range.
% (Ignore layers in which you do not get caught.)
% The severity of the whole trip is the sum of these values.
% In the example above, the trip severity is 0*3 + 6*4 = 24.
case_part1_a_test() -> 24 = first([{0, 3},{1, 2},{4, 4},{6, 4}]).
% Given the details of the firewall you've recorded, if you leave immediately, what is the severity of your whole trip?
case_part1_b_test() -> 632 = first(?PUZZLE).
% Now, you need to pass through the firewall without being caught - easier said than done.
% You can't control the speed of the packet, but you can delay it any number of picoseconds.
% For each picosecond you delay the packet before beginning your trip, all security scanners move one step.
% You're not in the firewall during this time; you don't enter layer 0 until you stop delaying the packet.
% In the example above, if you delay 10 picoseconds (picoseconds 0 - 9), you won't get caught:
% State after delaying:
%  0   1   2   3   4   5   6
% [ ] [S] ... ... [ ] ... [ ]
% [ ] [ ]         [ ]     [ ]
% [S]             [S]     [S]
%                 [ ]     [ ]
% Picosecond 10:
%  0   1   2   3   4   5   6
% ( ) [S] ... ... [ ] ... [ ]
% [ ] [ ]         [ ]     [ ]
% [S]             [S]     [S]
%                 [ ]     [ ]
%  0   1   2   3   4   5   6
% ( ) [ ] ... ... [ ] ... [ ]
% [S] [S]         [S]     [S]
% [ ]             [ ]     [ ]
%                 [ ]     [ ]
% Picosecond 11:
%  0   1   2   3   4   5   6
% [ ] ( ) ... ... [ ] ... [ ]
% [S] [S]         [S]     [S]
% [ ]             [ ]     [ ]
%                 [ ]     [ ]
%  0   1   2   3   4   5   6
% [S] (S) ... ... [S] ... [S]
% [ ] [ ]         [ ]     [ ]
% [ ]             [ ]     [ ]
%                 [ ]     [ ]
% Picosecond 12:
%  0   1   2   3   4   5   6
% [S] [S] (.) ... [S] ... [S]
% [ ] [ ]         [ ]     [ ]
% [ ]             [ ]     [ ]
%                 [ ]     [ ]
%  0   1   2   3   4   5   6
% [ ] [ ] (.) ... [ ] ... [ ]
% [S] [S]         [S]     [S]
% [ ]             [ ]     [ ]
%                 [ ]     [ ]
% Picosecond 13:
%  0   1   2   3   4   5   6
% [ ] [ ] ... (.) [ ] ... [ ]
% [S] [S]         [S]     [S]
% [ ]             [ ]     [ ]
%                 [ ]     [ ]
%  0   1   2   3   4   5   6
% [ ] [S] ... (.) [ ] ... [ ]
% [ ] [ ]         [ ]     [ ]
% [S]             [S]     [S]
%                 [ ]     [ ]
% Picosecond 14:
%  0   1   2   3   4   5   6
% [ ] [S] ... ... ( ) ... [ ]
% [ ] [ ]         [ ]     [ ]
% [S]             [S]     [S]
%                 [ ]     [ ]
%  0   1   2   3   4   5   6
% [ ] [ ] ... ... ( ) ... [ ]
% [S] [S]         [ ]     [ ]
% [ ]             [ ]     [ ]
%                 [S]     [S]
% Picosecond 15:
%  0   1   2   3   4   5   6
% [ ] [ ] ... ... [ ] (.) [ ]
% [S] [S]         [ ]     [ ]
% [ ]             [ ]     [ ]
%                 [S]     [S]
%  0   1   2   3   4   5   6
% [S] [S] ... ... [ ] (.) [ ]
% [ ] [ ]         [ ]     [ ]
% [ ]             [S]     [S]
%                 [ ]     [ ]
% Picosecond 16:
%  0   1   2   3   4   5   6
% [S] [S] ... ... [ ] ... ( )
% [ ] [ ]         [ ]     [ ]
% [ ]             [S]     [S]
%                 [ ]     [ ]
%  0   1   2   3   4   5   6
% [ ] [ ] ... ... [ ] ... ( )
% [S] [S]         [S]     [S]
% [ ]             [ ]     [ ]
%                 [ ]     [ ]
% Because all smaller delays would get you caught,
% the fewest number of picoseconds you would need to delay to get through safely is 10.
case_part2_a_test() -> 10 = second([{0, 3},{1, 2},{4, 4},{6, 4}]).
% What is the fewest number of picoseconds that you need to delay the packet to pass through the firewall without being caught?
case_part2_b_test_() -> {timeout, 60, fun() -> 3849742 = second(?PUZZLE) end}.