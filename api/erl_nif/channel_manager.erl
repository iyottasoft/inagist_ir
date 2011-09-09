-module(channel_manager).

-export([init/0, init_c/1, find_channels/1, test_twitter_timeline/0, test_twitter_timeline/1, test_init/0, test/0, test/1, test_file/0, stress_test/1]).

init() ->
  erlang:load_nif("../../lib/libchannel_manager_enif", 0).

init_c(_config_file) ->
  "NIF library not loaded for init_c".

find_channels(_tweet) ->
  "NIF library not loaded for find_channels".

test_twitter_timeline() ->
  "NIF library not loaded for test_twitter_timeline".

test_twitter_timeline(_user) ->
  "NIF library not loaded for test_twitter_timeline for single user".

test_from_file(_file_name) ->
  "NIF library not loaded for test_from_file".

test_init() ->
  init_c(<<"../../configs/channel_manager.config">>).

test() ->
  Tuple2_list = test_twitter_timeline(),
  case is_atom(Tuple2_list) of
    false -> false;
    true -> io:format("error")
  end,
  case is_list(Tuple2_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Tuple2_list]
  end.

test(_user) ->
  Tuple2_list = test_twitter_timeline(_user),
  case is_list(Tuple2_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Tuple2_list]
  end.

test_file() ->
  Tuple2_list = test_from_file(<<"../../data/lang/tweetsource/tweets/english.txt">>),
  %Tuple2_list = test_from_file(_file_name),
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
