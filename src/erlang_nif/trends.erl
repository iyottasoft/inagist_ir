-module(trends).

-export([init/0, init_c/0, get_trends/1, process_trends/1, test_init/0,  test/0, test_file/0]).

init() ->
  erlang:load_nif("../../lib/libtrends_erl", 0).

init_c() ->
  "NIF library not loaded".

get_trends(_text) ->
  "NIF library not loaded".

process_trends(_trends) ->
  "NIF library not loaded".

test_trends_twitter_timeline() ->
  "NIF library not loaded".

test_trends_file(_file_name) ->
  "NIF library not loaded for test_trends_file".

test_init() ->
  init_c().

test() ->
  Tuples_list = test_trends_twitter_timeline(),
  case is_atom(Tuples_list) of
    false -> false;
    true -> io:format("error")
  end,
  case is_list(Tuples_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Tuples_list]
  end.

test_file() ->
  Tuple2_list = test_trends_file(<<"../../data/trends/trends.txt">>),
  case is_atom(Tuple2_list) of
    false -> false;
    true -> io:format("error")
  end,
  case is_list(Tuple2_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Tuple2_list]
  end.

