-module(bedug).
-export([bedug/1]).
-include_lib("eunit/include/eunit.hrl").

case_a_test() -> 5 = bedug([0, 2, 7, 0]).

bedug(Banks) -> bedug(false, index(orddict:new(), 0, Banks), sets:new()).

bedug(true, Map, Seen) -> sets:size(Seen) + 1;
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

% The banks start with 0, 2, 7, and 0 blocks.
% The third bank has the most blocks, so it is chosen for redistribution.
% Starting with the next bank (the fourth bank) and then continuing to the first bank, the second bank, and so on,
% the 7 blocks are spread out over the memory banks.
% The fourth, first, and second banks get two blocks each, and the third bank gets one back.
% The final result looks like this: 2 4 1 2.
% Next, the second bank is chosen because it contains the most blocks (four).
% Because there are four memory banks, each gets one block. The result is: 3 1 2 3.
% Now, there is a tie between the first and fourth memory banks, both of which have three blocks.
% The first bank wins the tie, and its three blocks are distributed evenly over the other three banks, leaving it with none: 0 2 3 4.
% The fourth bank is chosen, and its four blocks are distributed such that each of the four banks receives one: 1 3 4 1.
% The third bank is chosen, and the same thing happens: 2 4 1 2.
% At this point, we've reached a state we've seen before: 2 4 1 2 was already seen.
% The infinite loop is detected after the fifth block redistribution cycle, and so the answer in this example is 5.