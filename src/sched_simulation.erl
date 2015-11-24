%% -*- erlang-indent-level: 2 -*-

%%%=============================================================================

-module(sched_simulation).

-export([enable/0, simulate/4]).

%%%=============================================================================

-export_type([state/0, result_fun/0]).

-opaque state() :: {state, term()}.

-type result_fun() :: fun(() -> term() | no_return()).

%%%=============================================================================

-spec enable() -> ok.

enable() ->
  sched_wrapper:enable_inspect({state, foo}).

%%%-----------------------------------------------------------------------------

-spec simulate(mfa(), [term()], term(), state()) -> {result_fun(), state()}.

simulate(MFA, Args, _Location, State) ->
  case MFA of
    {erlang, node, 0} ->
      {fun() -> node() end, State};
    {erlang, self, 0} ->
      {fun() -> self() end, State};
    _ ->
      {Module, Name, Arity} = MFA,
      erlang:display({wrapped, Module, Name, Arity}),
      {fun() -> erlang:apply(Module, Name, Args) end, State}
  end.

%%%=============================================================================
