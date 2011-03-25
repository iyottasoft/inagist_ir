-module(keytuples).

-export([init/0, init_c/3, getkeywords/1, gettuples/1, test_init/0, test_keywords/0, test_keywords/1, test_keywords_file/0, test_keywords_twitter_timeline/0, test_keywords_twitter_timeline/1, test_keytuples/0, test_keytuples/1, test_keytuples_file/0, test_keytuples_twitter_timeline/0, test_keytuples_twitter_timeline/1, stress_test_keywords/1, stress_test_keytuples/1]).

init() ->
  erlang:load_nif("../../lib/libkeytuples_erl", 0).

init_c(_stopwords_file_path, _dictionary_file_path, _unsafe_dictionary_file_path) ->
  "NIF library not loaded".

getkeywords(_tweet) ->
  "NIF library not loaded".

gettuples(_tweet) ->
  "NIF library not loaded".

test_keywords_twitter_timeline() ->
  "NIF library not loaded".

test_keywords_twitter_timeline(_user) ->
  "NIF library not loaded".

test_keywords_file(_file_name) ->
  "NIF library not loaded for test_keywords_file".

test_keytuples_twitter_timeline() ->
  "NIF library not loaded".

test_keytuples_twitter_timeline(_user) ->
  "NIF library not loaded".

test_keytuples_file(_file_name) ->
  "NIF library not loaded for test_from_lang_file".

test_init() ->
  init_c(<<"../../data/static_data/stopwords.txt">>,
         <<"../../data/static_data/dictionary.txt">>,
         <<"../../data/static_data/unsafe_dictionary.txt">>).

test_keywords() ->
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

test_keytuples() ->
  Tuples_list = test_keytuples_twitter_timeline(),
  case is_atom(Tuples_list) of
    false -> false;
    true -> io:format("error")
  end,
  case is_list(Tuples_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Tuples_list]
  end.

test_keytuples(_user) ->
  Tuples_list = test_keytuples_twitter_timeline(_user),
  case is_list(Tuples_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Tuples_list]
  end.

test_keytuples_file() ->
  Tuple2_list = test_keytuples_file(<<"../../data/lang/tweetsource/tweets/english.txt">>),
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

stress_test_keytuples([N]) ->
  Number = list_to_integer(atom_to_list(N)),
  lists:foreach(fun(X) -> timer:sleep(500), test_keytuples(), io:format("~p requests done~n",[X]) end, lists:seq(1,Number)).
