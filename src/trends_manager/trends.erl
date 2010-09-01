-module(trends).
-export([init/0, init_c/1, getkeywords/1, gettrends/1, test/2]).
init() ->
  erlang:load_nif("./trends", 0).
init_c(_stopwords_file_path) ->
  "NIF library not loaded".
getkeywords(_tweet) ->
  "NIF library not loaded".
gettrends(_user) ->
  "NIF library not loaded".
test(_tweet, _user) ->
  "NIF library not loaded".
