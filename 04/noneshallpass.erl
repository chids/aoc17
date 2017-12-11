-module(noneshallpass).
-export([noneshallpass/1]).
-include_lib("eunit/include/eunit.hrl").

case_a_test() -> 1 = noneshallpass([[aa, bb, cc, dd, ee]]).
case_b_test() -> 0 = noneshallpass([[aa, bb, cc, dd, aa]]).
case_c_test() -> 1 = noneshallpass([[aa, bb, cc, dd, aaa]]).

noneshallpass(Passphrases) -> lists:sum([ 1 || Phrase <- Passphrases, dedupe(Phrase) =:= Phrase]).

valid(Phrase, Phrase) -> Phrase;
valid(Phrase, _) -> false.

dedupe([])    -> [];
dedupe([H|T]) -> [H | [E || E <- dedupe(T), E =/= H]].

% A new system policy has been put in place
% that requires all accounts to use a passphrase instead of simply a password.
%
% A passphrase consists of a series of words (lowercase letters) separated by spaces.
% To ensure security, a valid passphrase must contain no duplicate words.
%
% For example:
%
% aa bb cc dd ee is valid.
% aa bb cc dd aa is not valid - the word aa appears more than once.
% aa bb cc dd aaa is valid - aa and aaa count as different words.
