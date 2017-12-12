-module(jumparound).
-export([jumparound/1]).
-include_lib("eunit/include/eunit.hrl").

case_a_test() -> 5 = jumparound([0, 3, 0, 1, -3]).

jumparound(Jumps) -> jumparound(index(dict:new(), 0, Jumps), 0, 0).

jumparound(Map, Count, Position) ->
  case dict:find(Position, Map) of
    error -> Count;
    {ok, Next} -> jumparound(dict:store(Position, Next + 1, Map), Count + 1, Position + Next)
  end.

index(Map, Index, [Jump|Jumps]) -> index(dict:store(Index, Jump, Map), Index + 1, Jumps);
index(Map, _, []) -> Map.
  
%  Positive jumps ("f,orward") move downward; negative jumps move upward.
%  For legibility in this example, these offset values will be written all on one line,
%  with the current instruction marked in parentheses.
%  The following steps would be taken before an exit is found:
%  (0) 3  0  1  -3
%    > before we have taken any steps.
%  (1) 3  0  1  -3
%    > jump with offset 0 (that is, don't jump at all). Fortunately, the instruction is then incremented to 1.
%   2 (3) 0  1  -3
%    > step forward because of the instruction we just modified. The first instruction is incremented again, now to 2.
%   2  4  0  1 (-3)
%    > jump all the way to the end; leave a 4 behind.
%   2 (4) 0  1  -2
%    > go back to where we just were; increment -3 to -2.
%   2  5  0  1  -2
%    > jump 4 steps forward, escaping the maze.
%  In this example, the exit is reached in 5 steps.