-module(jumparound).
-include_lib("eunit/include/eunit.hrl").


first(Jumps) -> jumparound(fun(_) -> 1 end, index(dict:new(), Jumps), 0, 0).
second(Jumps) -> jumparound(fun(Next) when Next >= 3 -> -1; (_) -> 1 end, index(dict:new(), Jumps), 0, 0).

jumparound(Offset, Map, Count, Position) ->
  case dict:find(Position, Map) of
    error -> Count;
    {ok, Next} -> Adjustment = Offset(Next),
      jumparound(Offset, dict:store(Position, Next + Adjustment, Map), Count + 1, Position + Next)
  end.

index(Map, [Jump|Jumps]) -> index(dict:store(dict:size(Map), Jump, Map), Jumps);
index(Map, []) -> Map.

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
case_part1_test() -> 5 = first([0, 3, 0, 1, -3]).
% Now, the jumps are even stranger:
%
% after each jump, if the offset was three or more, instead decrease it by 1.
% Otherwise, increase it by 1 as before.
%
% Using this rule with the above example, the process now takes 10 steps,
% and the offset values after finding the exit are left as 2 3 2 3 -1.
%
% How many steps does it now take to reach the exit?
case_part2_test() -> 10 = second([0, 3, 0, 1, -3]).