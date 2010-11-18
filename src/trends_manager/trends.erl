-module(trends).
-export([init/0, init_c/2, getkeywords/1, gettrends/1, test_twitter_timeline/0, test_twitter_timeline/1, test_init/0, test/0, test/1, stress_test/1]).

init() ->
  erlang:load_nif("../../lib/libtrends", 0).

init_c(_stopwords_file_path, _dictionary_file_path) ->
  "NIF library not loaded".

getkeywords(_tweet) ->
  "NIF library not loaded".

gettrends(_user) ->
  "NIF library not loaded".

test_twitter_timeline() ->
  "NIF library not loaded".

test_twitter_timeline(_user) ->
  "NIF library not loaded".

test_init() ->
  init_c(<<"../../data/static_data/stopwords.txt">>, <<"../../data/static_data/dictionary.txt">>).

test() ->
  %io:format("~p~n",[getkeywords(<<"Testing Keywords extract">>)]).
  Tuple3_list = test_twitter_timeline(),
  case is_list(Tuple3_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Tuple3_list]
  end.

test(_user) ->
  Tuple3_list = test_twitter_timeline(_user),
  case is_list(Tuple3_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Tuple3_list]
  end.

stress_test([N]) ->
  Number = list_to_integer(atom_to_list(N)),
  lists:foreach(fun(X) -> timer:sleep(500), test(), io:format("~p requests done~n",[X]) end, lists:seq(1,Number)).
