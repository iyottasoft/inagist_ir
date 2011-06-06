-module(profile).

-export([init/0, init_c/3, profile/2, profile/1, test_init/0, test/0, profile_from_file/1, test_file/0]).

init() ->
  erlang:load_nif("../../lib/libprofile_erl", 0).

init_c(_keytuples_extracter_config_file_path, _lang_detect_config_file_path, _channels_classifier_config_file_path) ->
  "NIF library not loaded".

profile(_twitter_handle) ->
  "NIF library for profile(_twitter_handle) not loaded".

profile(_twitter_handle, _output_file_name) ->
  "NIF library not loaded".

test_init() ->
  init_c(<<"../../configs/keytuples_extracter.config">>,
         <<"../../configs/language_detection.config">>,
         <<"../../configs/channels_classifier.config">>).

test() ->
  Tuples_list = profile(<<"balajiworld">>),
  case is_atom(Tuples_list) of
    false -> false;
    true -> io:format("error")
  end,
  case is_list(Tuples_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Tuples_list]
  end.

profile_from_file(_file_name) ->
  "NIF library not loaded to test profiler from file".

test_file() ->
  Tuple2_list = profile_from_file(<<"../../data/lang/tweetsource/tweets/english.txt">>),
  case is_atom(Tuple2_list) of
    false -> false;
    true -> io:format("error")
  end,
  case is_list(Tuple2_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Tuple2_list]
  end.

