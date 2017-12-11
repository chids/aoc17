-module(dubstep2).
-export([dubstep/0]).
-include_lib("eunit/include/eunit.hrl").
-define(STOP, 62).

solve_test() -> -312453 = dubstep().

dubstep() -> loop({0, 0, orddict:new()}, 0, 0, 1, right).

loop({_, _, Map}, _, _, ?STOP, _) ->
  {_, Max} = hd(orddict:to_list(orddict:fetch(hd(orddict:fetch_keys(Map)), Map))),
  Max;
loop(Map, Cycle, 0, Count, Heading) ->
  loop(Map, Cycle + 1, (Cycle + 1) * 2, Count, Heading);
loop(Map, Cycle, Step, Count, Heading) ->
  [NewHeading|_] = lists:reverse([Heading|[step(X) || X <- [Heading], mod(Count, Cycle) =:= 0]]),
  loop(log(Map, Heading), Cycle, Step - 1, Count + 1, NewHeading).

read({X, Y}, Trail) -> read(Y, orddict:find(-X, Trail));
read(Y, {ok, Column}) -> read(orddict:find(Y, Column));
read(_, error) -> 0.
read(error) -> 0;
read({ok, Value}) -> Value.

log({X, Y, Trail}, up) -> {X + 1, Y, append({X, Y}, Trail)};
log({X, Y, Trail}, down) -> {X - 1, Y, append({X, Y}, Trail)};
log({X, Y, Trail}, left) -> {X, Y - 1, append({X, Y}, Trail)};
log({X, Y, Trail}, right) -> {X, Y + 1, append({X, Y}, Trail)}.

append({X, Y}, Trail) ->
  N = sum_neighbours({X, Y}, Trail),
  orddict:update(-X, fun(E) -> orddict:store(Y, N, E) end, orddict:store(Y, N, orddict:new()), Trail).

sum_neighbours({X, Y}, Map) ->
  Neighbors = [{1, 0}, {-1, 0}, {0, 1}, {0, -1}, {1, -1}, {1, 1}, {-1, -1}, {-1, 1}],
  max(1, lists:sum([ read({X + A, Y + B}, Map) || {A, B} <- Neighbors])).

step(right) -> up;
step(up) -> left;
step(left) -> down;
step(down) -> right.

mod(X, Y) when X > 0 -> X rem Y;
mod(X, Y) when X < 0 -> Y + X rem Y;
mod(0, _) -> 0.

% As a stress test on the system, the programs here clear the grid and then store the value 1 in square 1.
% Then, in the same allocation order as shown above, they store the sum of the values in all adjacent squares,
% including diagonals.
% 
% So, the first few squares' values are chosen as follows:
% 
% Square 1 starts with the value 1.
% Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
% Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
% Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
% Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.
% Once a square is written, its value does not change.
% Therefore, the first few squares would receive the following values:
% 
% 147  142  133  122   59
% 304    5    4    2   57
% 330   10    1    1   54
% 351   11   23   25   26
% 362  747  806--->   ...
