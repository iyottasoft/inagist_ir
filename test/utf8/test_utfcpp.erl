-module(test_utfcpp).
-export([init/0, test_utfcpp/1, test_twitter_timeline/0, test_twitter_timeline/1, test/0, test/1, stress_test/1]).

init() ->
  erlang:load_nif("../../lib/libtestutfcpp_erl", 0).

test_utfcpp(_tweet) ->
  "NIF library not loaded".

test_twitter_timeline() ->
  "NIF library not loaded".

test_twitter_timeline(_user) ->
  "NIF library not loaded".

test() ->
  Lang_list = test_twitter_timeline(),
  case is_list(Lang_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Lang_list]
  end.

test(_user) ->
  Lang_list = test_twitter_timeline(_user),
  case is_list(Lang_list) of
    false -> false;
    true -> [io:format("~p~n",[X]) || X <- Lang_list]
  end.

stress_test([N]) ->
  Number = list_to_integer(atom_to_list(N)),
  lists:foreach(fun(X) -> timer:sleep(500), test(), io:format("~p requests done~n",[X]) end, lists:seq(1,Number)).
