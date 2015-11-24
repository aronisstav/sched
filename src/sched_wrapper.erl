%% -*- erlang-indent-level: 2 -*-

%% This module will never be instrumented.

%%%=============================================================================

-module(sched_wrapper).

-export([enable_inspect/1, inspect/3]).

%%%=============================================================================

-export_type([tag/0]).

-type tag() :: 'apply' | 'call' | 'receive'.

%%%=============================================================================

-spec enable_inspect(sched_simulation:state()) -> 'ok'.

enable_inspect(StateInfo) ->
  add_sched_state_info(StateInfo).

%%%-----------------------------------------------------------------------------

-spec inspect(tag(), [term()], term()) -> term(). %XXX Refine location?

inspect(Tag, Args, Location) ->
  Simulate =
    case remove_sched_state_info() of
      {true, StateInfo} ->
        case decide(Tag, Args, Location, StateInfo) of
          {true, Ret, NewStateInfo} ->
            add_sched_state_info(NewStateInfo),
            Ret;
          false ->
            add_sched_state_info(StateInfo),
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

add_sched_state_info(StateInfo) ->
  Old = proplists:lookup(sched, get()),
  put(sched, {sched, StateInfo, Old}),
  ok.

remove_sched_state_info() ->
  case get(sched) of
    {sched, StateInfo, Old} ->
      erase(sched),
      case Old of
        none -> ok;
        {sched, Value} -> put(sched, Value)
      end,
      {true, StateInfo};
    _ -> false
  end.

decide(apply, [Fun, ApplyArgs], Location, StateInfo) ->
  case is_function(Fun) of
    true ->
      Module = get_fun_info(Fun, module),
      Name = get_fun_info(Fun, name),
      Arity = get_fun_info(Fun, arity),
      case erlang:is_builtin(Module, Name, Arity) of
        true ->
          decide(call, [Module, Name, ApplyArgs], Location, StateInfo);
        false ->
          ok = sched_loader:load(Module),
          false
      end;
    false ->
      false
  end;
decide(call, [Module, Name, CallArgs], Location, StateInfo) ->
  case erlang:is_builtin(Module, Name, length(CallArgs)) of
    true ->
      Arity = length(CallArgs),
      MFA = {Module, Name, Arity},
      {Ret, NewStateInfo} =
        sched_simulation:simulate(MFA, CallArgs, Location, StateInfo),
      {true, Ret, NewStateInfo};
    false ->
      ok = sched_loader:load(Module),
      false
  end;
decide(_, _, _, _) ->
  false.

get_fun_info(Fun, Tag) ->
  {Tag, Info} = erlang:fun_info(Fun, Tag),
  Info.
