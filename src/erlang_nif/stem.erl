-module(stem).
-export([init/0, init_c/3, stem/1, test_twitter_timeline/0, test_twitter_timeline/1, test_init/0, test/0, test/1, stress_test/1]).

init() ->
  erlang:load_nif("../../lib/libstemmer_erl", 0).

init_c(_stopwords_file_path, _dictionary_file_path, _stemmer_dictionary_file_path) ->
  "NIF library not loaded".

stem(_tweet) ->
  "NIF library not loaded".

test_twitter_timeline() ->
  "NIF library not loaded".

test_twitter_timeline(_user) ->
  "NIF library not loaded".

test_init() ->
  init_c(<<"../../data/static_data/stopwords.txt">>, <<"../../data/static_data/dictionary.txt">>, <<"../../data/static_data/stemmer_dictionary.txt">>).

test() ->
  Test_list = test_twitter_timeline(),
  case is_list(Test_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Test_list]
  end.

test(_user) ->
  Test_list = test_twitter_timeline(_user),
  case is_list(Test_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Test_list]
  end.

stress_test([N]) ->
  Number = list_to_integer(atom_to_list(N)),
  lists:foreach(fun(X) -> timer:sleep(500), test(), io:format("~p requests done~n",[X]) end, lists:seq(1,Number)).
