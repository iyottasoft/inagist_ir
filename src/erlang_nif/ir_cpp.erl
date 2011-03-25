-module(ir_cpp).

-export([init/0, init_c/5, getkeywords/1, getlang/1, gettrends/1, test_init/0, test_keywords/0, test_keywords/1, test_keywords_file/0, test_keywords_twitter_timeline/0, test_keywords_twitter_timeline/1, test_lang/0, test_lang/1, test_lang_file/0, test_lang_twitter_timeline/0, test_lang_twitter_timeline/1, test_trends_file/0, stress_test_keywords/1]).

init() ->
  erlang:load_nif("../../lib/libir_cpp_erl", 0).

init_c(_stopwords_file_path, _dictionary_file_path, _unsafe_dictionary_file_path, _lang_detect_config_file_path, _channels_dictionary_file_path) ->
  "NIF library not loaded".

getkeywords(_tweet) ->
  "NIF library not loaded".

getlang(_tweet) ->
  "NIF library not loaded".

gettrends(_keywords) ->
  "NIF library not loaded".

test_keywords_twitter_timeline() ->
  "NIF library not loaded".

test_keywords_twitter_timeline(_user) ->
  "NIF library not loaded".

test_keywords_file(_file_name) ->
  "NIF library not loaded for test_keywords_file".

test_lang_twitter_timeline() ->
  "NIF library not loaded".

test_lang_twitter_timeline(_user) ->
  "NIF library not loaded".

test_lang_file(_file_name) ->
  "NIF library not loaded for test_from_lang_file".

test_trends_file(_file_name) ->
  "NIF library not loaded for test_trends_from_file".

test_init() ->
  init_c(<<"../../data/static_data/stopwords.txt">>,
         <<"../../data/static_data/dictionary.txt">>,
         <<"../../data/static_data/unsafe_dictionary.txt">>,
         <<"../../configs/language_detection.config">>,
         <<"../../data/static_data/channels_dictionary.txt">>).

test_keywords() ->
  %io:format("~p~n",[getkeywords(<<"Testing Keywords extract">>)]).
  Tuples_list = test_keywords_twitter_timeline(),
  case is_atom(Tuples_list) of
    false -> false;
    true -> io:format("error")
  end,
  case is_list(Tuples_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Tuples_list]
  end.

test_keywords(_user) ->
  Tuples_list = test_keywords_twitter_timeline(_user),
  case is_list(Tuples_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Tuples_list]
  end.

test_keywords_file() ->
  Tuple2_list = test_keywords_file(<<"../../data/lang/tweetsource/tweets/english.txt">>),
  case is_atom(Tuple2_list) of
    false -> false;
    true -> io:format("error")
  end,
  case is_list(Tuple2_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Tuple2_list]
  end.

test_lang() ->
  Tuples_list = test_lang_twitter_timeline(),
  case is_atom(Tuples_list) of
    false -> false;
    true -> io:format("error")
  end,
  case is_list(Tuples_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Tuples_list]
  end.

test_lang(_user) ->
  Tuples_list = test_lang_twitter_timeline(_user),
  case is_list(Tuples_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Tuples_list]
  end.

test_lang_file() ->
  Tuple2_list = test_lang_file(<<"../../data/lang/tweetsource/tweets/english.txt">>),
  case is_atom(Tuple2_list) of
    false -> false;
    true -> io:format("error")
  end,
  case is_list(Tuple2_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Tuple2_list]
  end.

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

stress_test_keywords([N]) ->
  Number = list_to_integer(atom_to_list(N)),
  lists:foreach(fun(X) -> timer:sleep(500), test_keywords(), io:format("~p requests done~n",[X]) end, lists:seq(1,Number)).
