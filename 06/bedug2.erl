-module(bedug2).
-export([bedug/1]).
-include_lib("eunit/include/eunit.hrl").

case_a_test() -> 4 = bedug([2, 4, 1, 2]).

bedug(Banks) -> bedug(false, index(orddict:new(), 0, Banks), sets:new()).

bedug(true, _, Seen) -> sets:size(Seen);
bedug(false, Map, Seen) ->
  {K, V, Map2} = pop(Map),
  AA = spread(Map2, {K + 1, V}, orddict:size(Map2)),
  bedug(sets:is_element(AA, Seen), AA, sets:add_element(AA, Seen)).

spread(Map, {_, 0}, _) -> Map;
spread(Map, {K1, V}, Size) ->
  K2 = mod(K1, Size),
  spread(orddict:store(K2, orddict:fetch(K2, Map) + 1, Map), {K2 + 1, V - 1}, Size).

pop(Map) ->
  {K, V} = orddict:fold(
    fun(K1, V1, {_, V2}) when V1 > V2 ->
      {K1, V1};
      (_, _, {K2, V2}) -> {K2, V2}
    end, {0, 0}, Map),
  {K, V, orddict:store(K, 0, Map)}.

index(Map, Index, [Block|Blocks]) -> index(orddict:store(Index, Block, Map), Index + 1, Blocks);
index(Map, _, []) -> Map.

mod(X, Y) when X > 0 -> X rem Y;
mod(X, Y) when X < 0 -> Y + X rem Y;
mod(0, _) -> 0.

% Out of curiosity, the debugger would also like to know the size of the loop:
% starting from a state that has already been seen,
% how many block redistribution cycles must be performed before that same state is seen again?
%
% In the example above, 2 4 1 2 is seen again after four cycles, and so the answer in that example would be 4.
% How many cycles are in the infinite loop that arises from the configuration in your puzzle input?