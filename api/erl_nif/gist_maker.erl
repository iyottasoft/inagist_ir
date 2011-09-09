-module(gist_maker).

-export([init/0, init_c/1, get_gist/1, test_init/0, test/0, test_user/1, test_file/1, test_gist_twitter_timeline/0, test_gist_twitter_timeline/1, stress_test/1]).

init() ->
  erlang:load_nif("../../lib/libgist_maker_enif", 0).

init_c(_keytuples_extracter_config_file_path) ->
  "NIF library not loaded".

get_gist(_text) ->
  "NIF library not loaded".

test_gist_twitter_timeline() ->
  "NIF library not loaded".

test_gist_twitter_timeline(_user) ->
  "NIF library not loaded".

test_gist_file(_file_name) ->
  "NIF library not loaded for test_gist_file".

test_init() ->
  init_c(<<"../../configs/gist_maker.config">>).

test() ->
  Tuples_list = test_gist_twitter_timeline(),
  case is_atom(Tuples_list) of
    false -> false;
    true -> io:format("error")
  end,
  case is_list(Tuples_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Tuples_list]
  end.

test_user(_user) ->
  Tuples_list = test_gist_twitter_timeline(_user),
  case is_list(Tuples_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Tuples_list]
  end.

test_file(_file) ->
  Tuple2_list = test_gist_file(_file),
  case is_atom(Tuple2_list) of
    false -> false;
    true -> io:format("error")
  end,
  case is_list(Tuple2_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Tuple2_list]
  end.

stress_test([N]) ->
  Number = list_to_integer(atom_to_list(N)),
  lists:foreach(fun(X) -> timer:sleep(500), test(), io:format("~p requests done~n",[X]) end, lists:seq(1,Number)).
