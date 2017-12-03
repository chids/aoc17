-module(excel2).
-author("marten.gustafson@gmail.com").
-export([excel2/1]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

excel2(Spreadsheet) -> excel(0, Spreadsheet).

excel(Checksum, []) -> Checksum;
excel(Checksum, [Head|Tail]) ->
  Row = array:from_list(Head),
  excel(Checksum + row(0, 0, Row), Tail).

row(Index, Sum, Row) when Sum == 0 ->
  Current = array:get(Index, Row),
  Result = array:map(fun(Pos, Value) ->
    case Current rem Value of
      0 when Pos /= Index -> round(Current / Value);
      _ -> 0
    end
  end, Row),
  row(Index + 1, Sum + lists:sum(array:to_list(Result)), Row);
row(_, Sum, _) -> Sum.

-ifdef(TEST).
case_one_test() -> 4 = excel2([[5, 9, 2, 8]]).
case_two_test() -> 9 = excel2([[5, 9, 2, 8], [9, 4, 7, 3], [3, 8, 6, 5]]).
-endif.

% It sounds like the goal is to find the only two numbers in each row where one evenly divides the other - that is,
% where the result of the division operation is a whole number. They would like you to find those numbers on each line,
% divide them, and add up each line's result.
%
% For example, given the following spreadsheet:
%
% 5 9 2 8
% 9 4 7 3
% 3 8 6 5
% In the first row, the only two numbers that evenly divide are 8 and 2; the result of this division is 4.
% In the second row, the two numbers are 9 and 3; the result is 3.
% In the third row, the result is 2.
% In this example, the sum of the results would be 4 + 3 + 2 = 9.