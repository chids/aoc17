-module(dubstep).
-export([dubstep/1]).
-include_lib("eunit/include/eunit.hrl").

case_a_test() -> 0 = dubstep(1).
case_b_test() -> 3 = dubstep(12).
case_c_test() -> 2 = dubstep(23).
case_d_test() -> 31 = dubstep(1024).

dubstep(Stop) -> loop({0, 0}, 0, 0, lists:seq(1, Stop - 1), right).

loop(Distance, Cycle, 0, N, Heading) ->
  loop(Distance, Cycle + 1, (Cycle + 1) * 2, N, Heading);
loop(Distance, Cycle, Step, [N|Ns], Heading) ->
  [NewHeading|_] = lists:reverse([Heading|[step(X) || X <- [Heading], mod(N, Cycle) =:= 0]]),
  loop(log(Distance, Heading), Cycle, Step - 1, Ns, NewHeading);
loop({X, Y}, _, _, [], _) -> abs(X) + abs(Y).

log({X, Y}, up) -> {X + 1, Y};
log({X, Y}, down) -> {X - 1, Y};
log({X, Y}, left) -> {X, Y - 1};
log({X, Y}, right) -> {X, Y + 1}.

step(right) -> up;
step(up) -> left;
step(left) -> down;
step(down) -> right.

mod(X, Y) when X > 0 -> X rem Y;
mod(X, Y) when X < 0 -> Y + X rem Y;
mod(0, _) -> 0.

%  17  16  15  14  13
%  18   5   4   3  12
%  19   6   1   2  11 ..
%  20   7   8   9  10 27
%  21  22  23   24 25 26

% While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1
% (the location of the only access port for this memory system)
% by programs that can only move up, down, left, or right.
%
% They always take the shortest path: the Manhattan Distance between the location of the data and square 1.
% 
% For example:
% 
% Data from square 1 is carried 0 steps, since it's at the access port.
% Data from square 12 is carried 3 steps, such as: down, left, left.
% Data from square 23 is carried only 2 steps: up twice.
% Data from square 1024 must be carried 31 steps.
% How many steps are required to carry the data from the square
% identified in your puzzle input all the way to the access port?
