-module(incha).
-author("marten.gustafson@gmail.com").
-export([incha/1]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

incha(Numbers) ->
  incha(0, [hd(Numbers)|lists:reverse(Numbers)]).

incha(State, [Head|Tail]) -> 
  incha(State + check(Head, Tail), Tail);
incha(State, []) -> State.

check(_, []) -> 0;
check(_, false) -> 0;
check(Head, true) -> Head;
check(Head, Tail) -> check(Head, Head =:= hd(Tail)).

-ifdef(TEST).
case_one_test() -> 3 = incha([1,1,2,2]).
case_two_test() -> 4 = incha([1,1,1,1]).
case_three_test() -> 0 = incha([1,2,3,4]).
case_four_test() -> 9 = incha([9,1,2,1,2,1,2,9]).
-endif.

% The captcha requires you to review a sequence of digits (your puzzle input) and find the
% sum of all digits that match the next digit in the list. The list is circular, so the
% digit after the last digit is the first digit in the list.
% 
% 1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit and the third digit (2) matches the fourth digit.
% 1111 produces 4 because each digit (all 1) matches the next.
% 1234 produces 0 because no digit matches the next.
% 91212129 produces 9 because the only digit that matches the next one is the last digit, 9.