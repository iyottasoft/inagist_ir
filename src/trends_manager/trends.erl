-module(trends).
-export([init/0, init_c/2, getkeywords/1, gettrends/1, test_twitter_timeline/0, test/2, test/0]).
init() ->
  erlang:load_nif("../../lib/libtrends", 0).
init_c(_stopwords_file_path, _dictionary_file_path) ->
  "NIF library not loaded".
getkeywords(_tweet) ->
  "NIF library not loaded".
gettrends(_user) ->
  "NIF library not loaded".
test_twitter_timeline() ->
  "NIF library not loaded".
test(_tweet, _user) ->
  "NIF library not loaded".
test() ->
  Tweets = test_twitter_timeline(),
  [getkeywords(X) || X <- Tweets].
