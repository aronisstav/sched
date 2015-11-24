-module(proper_test).

-export([test/0]).

-spec test() -> ok.

test() ->
  erlang:display(self()),
  erlang:display(bar(self())),
  erlang:display(bar(foo)),
  proper_gen:pick(proper_types:integer()),
  io:format("Ban ~p!", [me]),
  ok.

bar(Foo) when Foo =:= self() ->
  ok;
bar(_) ->
  error.
