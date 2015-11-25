%% -*- erlang-indent-level: 2 -*-

%%%=============================================================================

-module(sched_simulation).

-export([enable/0, simulate_mfa/4, simulate_receive/4]).

-export_type([state/0]).

%%%=============================================================================

-include("sched.hrl").

%%%-----------------------------------------------------------------------------

-define(s(R), {same_state, R}).
-define(u(U, R), {new_state, U, R}).

%%%=============================================================================

-record(state, {
          infinity_after = infinity    :: timeout(),
          messages       = queue:new() :: message_queue(),
          self           = self()      :: pid()
         }).

-opaque state() :: #state{}.

-type result_fun()          :: fun(() -> term() | no_return()).

-type location()            :: sched_loader:location().
-type receive_pattern_fun() :: sched_loader:receive_pattern_fun().

-ifdef(BEFORE_OTP_17).
-type message_queue() :: queue().
-else.
-type message_queue() :: queue:queue(term()).
-endif.

%%%=============================================================================

-spec enable() -> ok.

enable() ->
  sched_wrapper:enable_inspect(#state{}).

%%%-----------------------------------------------------------------------------

-spec simulate_mfa(mfa(), [term()], location(), state()) ->
                      {result_fun(), state()}.

simulate_mfa(MFA, Args, Location, State) ->
  case skip_report(MFA, State) of
    true ->
      {same_state, Result} = mfa(MFA, Args, State),
      {fun() -> Result end, State};
    false ->
      UpdatedState = wait_signal(State),
      {BaseMFA, NewArgs} = canonicalize(MFA, Args),
      {Result, NewState} =
        try
          mfa(BaseMFA, NewArgs, UpdatedState)
        of
          ?u(U, Value) ->
            {{ok, Value}, U};
          ?s(Value) ->
            {{ok, Value}, UpdatedState}
        catch
          Class:Reason ->
            {{raise, Class, Reason}, UpdatedState}
        end,
      report_event(MFA, Args, Result, Location, NewState),
      SimFun =
        case Result of
          {ok, V} ->
            fun() -> V end;
          {raise, C, R} ->
            fun() ->
                {Module, Name, _Arity} = MFA,
                _ = (catch throw(reset_stack_here)),
                [_|S] = erlang:get_stacktrace(),
                erlang:raise(C, R, [{Module, Name, Args}|S])
            end
        end,
      {SimFun, NewState}
  end.

-spec simulate_receive(receive_pattern_fun(), timeout(), location(), state()) ->
                          state().

simulate_receive(_PatternFun, Timeout, _Location, State) ->
  _RealTimeout =
    case Timeout >= State#state.infinity_after of
      false -> Timeout;
      true -> infinity
    end,
  State.

%%%=============================================================================

skip_report({erlang, self, 0}, _State) -> true;
skip_report(_Other, _State) -> false.

%%%-----------------------------------------------------------------------------

canonicalize({erlang, '!', 2}, Args) ->
  {{erlang, send, 2}, Args};
canonicalize(MFA, Args) ->
  {MFA, Args}.

%%%=============================================================================

mfa(MFA, Args, State) ->
  case MFA of
    {erlang, monitor, 2} ->
      error(badarg);
    {erlang, self, 0} ->
      ?s(State#state.self);
    _ ->
      {Module, Name, _Arity} = MFA,
      ?s(erlang:apply(Module, Name, Args))
  end.

%%%=============================================================================

wait_signal(State) ->

  State.

report_event(_MFA, _Args, _Result, _Location, _State) ->
  ?d({report, _MFA}),
  ok.
