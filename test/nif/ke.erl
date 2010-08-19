-module(ke).
-export([init/0, run/0]).
init() ->
  erlang:load_nif("./ke", 0).
run() ->
  "NIF library not loaded".
