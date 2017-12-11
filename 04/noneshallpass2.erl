-module(noneshallpass2).
-export([noneshallpass/1]).
-include_lib("eunit/include/eunit.hrl").

case_a_test() -> 1 = noneshallpass([[abcde, fghij]]).
case_b_test() -> 0 = noneshallpass([[abcde, xyz, ecdab]]).
case_c_test() -> 2 = noneshallpass([[abcde, fghij], [abcde, xyz, ecdab], [abcde, fghij]]).

noneshallpass(Passphrases) -> noneshallpass(0, Passphrases).

noneshallpass(Match, [Phrase|Passphrases]) ->
  Sorted = [lists:sort(atom_to_list(Characters)) || Characters <- Phrase],
  noneshallpass(Match + max(0, lists:sum([ 1 || _ <- dedupe(Sorted)]) div lists:sum([ 1 || _ <- Sorted])), Passphrases);
noneshallpass(Match, []) -> Match.

dedupe([])    -> [];
dedupe([H|T]) -> [H | [E || E <- dedupe(T), E /= H]].

% For added security, yet another system policy has been put in place.
% Now, a valid passphrase must contain no two words that are anagrams of each other - that is,
% a passphrase is invalid if any word's letters can be rearranged to form any other word in the passphrase.
% 
% For example:
% 
% abcde fghij is a valid passphrase.
% abcde xyz ecdab is not valid - the letters from the third word can be rearranged to form the first word.
% a ab abc abd abf abj is a valid passphrase, because all letters need to be used when forming another word.
% iiii oiii ooii oooi oooo is valid.
% oiii ioii iioi iiio is not valid - any of these words can be rearranged to form any other word.