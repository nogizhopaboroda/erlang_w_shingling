-module(shingles).

-export([similarity/2, similarity/3]).

-define(STOP_WORDS, [
  "the", "and", "a", "&", "!", ",", "."
]).

-define(SPLIT_SYMBOLS, " &!,.").

-define(SHINGLE_LENGTH, 1).


similarity(Text1, Text2) ->
  similarity(Text1, Text2, ?SHINGLE_LENGTH).

similarity(Text1, Text2, ShingleLength) ->
  compare(
    genshingle(canonize(tokenize(Text1)), ShingleLength),
    genshingle(canonize(tokenize(Text2)), ShingleLength)
  ).


%% PRIVATE

ngram(List, Length) ->
  ngram(List, Length, []).

ngram(List, Length, Accum) ->
  {Ngram, Remainder} = lists:split(Length, List),
  case Remainder of
    [] -> Accum ++ [Ngram];
    _ -> ngram(tl(List), Length, Accum ++ [Ngram])
  end.


genshingle(String, ShingleLength) ->
  genshingle(ngram(String, ShingleLength), ShingleLength, []).

genshingle([], _, Accum) -> Accum;
genshingle([Head|String], ShingleLength, Accum) ->
  Hash = erlang:crc32(unicode:characters_to_binary(Head,utf8,utf8)),
  genshingle(String, ShingleLength, Accum ++ [Hash]).


tokenize(String) ->
  string:tokens(String, ?SPLIT_SYMBOLS).


canonize(WordsList) ->
  WordsList -- ?STOP_WORDS.


compare(Source1, Source2) ->
  IntersectionCount = length([X || X <- Source1, Y <- Source2, X == Y]),
  IntersectionCount * 2 / (length(Source1) + length(Source2)) * 100.
