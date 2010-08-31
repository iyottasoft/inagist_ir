-module(trends).
-export([init/0, init_c/0, submit/0, trends/0, deinit_c/0, getkeywords/3, gettrends/1, test/1]).
init() ->
  erlang:load_nif("./trends", 0).
init_c() ->
  "NIF library not loaded".
submit() ->
  "NIF library not loaded".
trends() ->
  "NIF library not loaded".
deinit_c() ->
  "NIF library not loaded".
getkeywords(_user, _tweet, _keywords) ->
  "NIF library not loaded".
gettrends(_user) ->
  "NIF library not loaded".
test(_tweet) ->
  "NIF library not loaded".
