-module(jumparound2).
-export([jumparound/1]).
-include_lib("eunit/include/eunit.hrl").

case_a_test() -> 10 = jumparound([0, 3, 0, 1, -3]).

jumparound(Jumps) -> jumparound(index(dict:new(), 0, Jumps), 0, 0).

jumparound(Map, Count, Position) ->
  case dict:find(Position, Map) of
    error -> Count;
    {ok, Next} -> Adjustment = case Next of
        Next when Next >= 3 -> -1;
        Next -> 1
      end,
      jumparound(dict:store(Position, Next + Adjustment, Map), Count + 1, Position + Next)
  end.

index(Map, Index, [Jump|Jumps]) -> index(dict:store(Index, Jump, Map), Index + 1, Jumps);
index(Map, _, []) -> Map.

% Now, the jumps are even stranger:
%
% after each jump, if the offset was three or more, instead decrease it by 1.
% Otherwise, increase it by 1 as before.
%
% Using this rule with the above example, the process now takes 10 steps,
% and the offset values after finding the exit are left as 2 3 2 3 -1.
%
% How many steps does it now take to reach the exit?