-module(trends).

-export([init/0, init_c/0, gettrends/1, test_init/0, test_trends_file/0]).

init() ->
  erlang:load_nif("../../lib/libtrends_erl", 0).

init_c() ->
  "NIF library not loaded".

gettrends(_keywords) ->
  "NIF library not loaded".

test_trends_file(_file_name) ->
  "NIF library not loaded for test_trends_file".

test_init() ->
  init_c().

test_trends_file() ->
  Tuple2_list = test_trends_file(<<"../../data/trends/trends.txt">>),
  case is_atom(Tuple2_list) of
    false -> false;
    true -> io:format("error")
  end,
  case is_list(Tuple2_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Tuple2_list]
  end.

