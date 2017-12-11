-module(excel).
-author("marten.gustafson@gmail.com").
-export([excel/1]).
-include_lib("eunit/include/eunit.hrl").

case_one_test() -> 18 = excel([[5, 1, 9, 5], [7, 5, 3], [2, 4, 6, 8]]).

excel(Spreadsheet) -> excel(0, Spreadsheet).
excel(Checksum, [Row|Rows]) -> excel(Checksum + (lists:max(Row) - lists:min(Row)), Rows);
excel(Checksum, []) -> Checksum.

% For example, given the following spreadsheet:
% 
% 5 1 9 5
% 7 5 3
% 2 4 6 8
% The first row's largest and smallest values are 9 and 1, and their difference is 8.
% The second row's largest and smallest values are 7 and 3, and their difference is 4.
% The third row's difference is 6.
% In this example, the spreadsheet's checksum would be 8 + 4 + 6 = 18.