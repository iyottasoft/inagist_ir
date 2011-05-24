-module(profile).

-export([init/0, init_c/3, profile/2, profile/1, test_init/0, test/0]).

init() ->
  erlang:load_nif("../../lib/libprofile_erl", 0).

init_c(_keytuples_extracter_config_file_path, _lang_detect_config_file_path, _text_classifier_config_file_path) ->
  "NIF library not loaded".

profile(_twitter_handle) ->
  "NIF library for profile(_twitter_hanlde) not loaded".

profile(_twitter_handle, _output_file_name) ->
  "NIF library not loaded".

test_init() ->
  init_c(<<"../../configs/keytuples_extracter.config">>,
         <<"../../configs/language_detection.config">>,
         <<"../../configs/text_classifier.config">>).

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

