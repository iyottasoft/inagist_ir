-module(trends).
-export([init/0, start/0, getkeywords/1, test/1]).
init() ->
  erlang:load_nif("./trends", 0).
start() ->
  "NIF library not loaded".
getkeywords(_tweet) ->
  "NIF library not loaded".
test(_tweet) ->
  "NIF library not loaded".
