-module(trends).
-export([init/0, submit/0, trends/0, getkeywords/2, gettrends/1, test/1]).
init() ->
  erlang:load_nif("./trends", 0).
submit() ->
  "NIF library not loaded".
trends() ->
  "NIF library not loaded".
getkeywords(_user, _tweet) ->
  "NIF library not loaded".
gettrends(_user) ->
  "NIF library not loaded".
test(_tweet) ->
  "NIF library not loaded".
