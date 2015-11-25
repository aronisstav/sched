%% -*- erlang-indent-level: 2 -*-

%% This module will never be instrumented.

%%%=============================================================================

-module(sched_wrapper).

-export([enable_inspect/1, inspect/3]).

-include("sched.hrl").

%%%=============================================================================

-export_type([tag/0]).

-type tag() :: 'apply' | 'call' | 'receive'.

%%%=============================================================================

-spec enable_inspect(sched_simulation:state()) -> 'ok'.

enable_inspect(State) ->
  add_sched_state_info(State).

%%%-----------------------------------------------------------------------------

-spec inspect(tag(), [term()], sched_event:location()) -> term().

inspect(Tag, Args, Location) ->
  Simulate =
    case remove_sched_state_info() of
      {true, State} ->
        case decide(Tag, Args, Location, State) of
          {true, Ret, NewState} ->
            add_sched_state_info(NewState),
            Ret;
          false ->
            add_sched_state_info(State),
            false
        end;
      false ->
        false
    end,
  case Simulate of
    false ->
      case {Tag, Args} of
        {apply, [Fun, ApplyArgs]} ->
          erlang:apply(Fun, ApplyArgs);
        {call, [Module, Name, CallArgs]} ->
          erlang:apply(Module, Name, CallArgs);
        {'receive', [_, Timeout]} ->
          Timeout
      end;
    SimFun ->
      SimFun()
  end.

%%%=============================================================================

add_sched_state_info(State) ->
  Old = proplists:lookup(sched, get()),
  put(sched, {sched, State, Old}),
  ok.

remove_sched_state_info() ->
  case get(sched) of
    {sched, State, Old} ->
      erase(sched),
      case Old of
        none -> ok;
        {sched, Value} -> put(sched, Value)
      end,
      {true, State};
    _ -> false
  end.

decide('apply', [Fun, ApplyArgs], Location, State) ->
  case is_function(Fun) of
    true ->
      Module = get_fun_info(Fun, module),
      Name = get_fun_info(Fun, name),
      Arity = get_fun_info(Fun, arity),
      case erlang:is_builtin(Module, Name, Arity) of
        true ->
          decide(call, [Module, Name, ApplyArgs], Location, State);
        false ->
          ok = sched_loader:load(Module),
          false
      end;
    false ->
      false
  end;
decide('call', [Module, Name, CallArgs], Location, State) ->
  case erlang:is_builtin(Module, Name, length(CallArgs)) of
    true ->
      Arity = length(CallArgs),
      MFA = {Module, Name, Arity},
      {Ret, NewState} =
        sched_simulation:simulate_mfa(MFA, CallArgs, Location, State),
      {true, Ret, NewState};
    false ->
      ok = sched_loader:load(Module),
      false
  end;
decide('receive', [PatternFun, Timeout], Location, State) ->
  NewState =
    sched_simulation:simulate_receive(PatternFun, Timeout, Location, State),
  {true, fun() -> Timeout end, NewState}.

get_fun_info(Fun, Tag) ->
  {Tag, Info} = erlang:fun_info(Fun, Tag),
  Info.
