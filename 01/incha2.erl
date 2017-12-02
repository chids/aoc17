-module(incha2).
-author("marten.gustafson@gmail.com").
-export([incha2/1]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

incha2(Numbers) ->
  Array = array:from_list(Numbers),
  incha(array:size(Array) - 1, 0, Array).

incha(-1, State, _) -> State;
incha(Counter, State, Numbers) -> incha(Counter - 1, State + check(Counter, Numbers), Numbers).

check(Counter, Numbers) ->
  Size = array:size(Numbers),
  Partner = (Counter + (Size div 2)) rem Size,
  result(array:get(Counter, Numbers), array:get(Partner, Numbers)).

result(Value, Value) -> Value;
result(_, _) -> 0.

-ifdef(TEST).
case_one_test() -> 6 = incha2([1,2,1,2]).
case_two_test() -> 0 = incha2([1,2,2,1]).
case_three_test() -> 4 = incha2([1,2,3,4,2,5]).
case_four_test() -> 12 = incha2([1,2,3,1,2,3]).
case_five_test() -> 4 = incha2([1,2,1,3,1,4,1,5]).
-endif.

% Now, instead of considering the next digit, it wants you to consider the digit halfway around the circular list.
% That is, if your list contains 10 items, only include a digit in your sum if the digit 10/2 = 5 steps forward matches it.
% Fortunately, your list has an even number of elements.
%
% For example:
%
% 1212 produces 6: the list contains 4 items, and all four digits match the digit 2 items ahead.
% 1221 produces 0, because every comparison is between a 1 and a 2.
% 123425 produces 4, because both 2s match each other, but no other digit has a match.
% 123123 produces 12.
% 12131415 produces 4.